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
      ******************************************************************
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCL0031BM.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 点数金額モジュールから病名登録処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/08/XX    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   02/04/30  改正に伴う修正
      *  01.00.02    NACL-多々納  03/05/07  投薬を院内へ
      *  01.00.03    NACL-藤原    03/09/30  病名コードの取り込み
      *  01.00.04    NACL-藤原    04/01/29  転帰区分の編集
      *  01.00.05    NACL-多々納  04/02/27  保険修正
      *  01.00.06    NACL-藤原    04/03/19  病名検索にサブパラメタ追加
      *  01.00.07    NACL-藤原    04/03/22  更新エラー時の処理終了
      *  01.00.08    NACL-多々納  04/03/29  外用回数編集修正
      *  01.00.09    NACL-多々納  04/08/24  入院回数修正
      *  01.00.10    NACL-多々納  04/10/21  ドクター追加
      *  01.00.11    NACL-多々納  04/11/09  入院追加
      *  01.00.12    NACL-藤原    05/03/10  病名コードの取り込み
      *                                     未コード化傷病名コードの場合
      *  01.00.13    NACL-藤原    05/05/25  疑いフラグ、主病名フラグによ
      *                                     る病名の更新
      *  01.00.14    NACL-藤原    05/08/25  更新時の疑いフラグの設定
      *  01.00.15    NACL-多々納  05/09/26  加算のみの剤を有効とする
      *  01.00.16    NACL-多々納  05/12/06  受付対応
      *                                     MONFUNC対応
      *  01.00.17    NACL-多々納  06/01/16  乳幼児特例対応
      *  01.00.18    NACL-藤原    06/01/25  疑いフラグからの病名の編集方
      *                                     法の変更
      *  01.00.19    NACL-多々納  06/07/03  置換え処理追加他
      *  03.05.00    NACL-多々納  07/05/10  グループ診療対応
      *  03.05.00    NACL-多々納  07/05/25  UIDテーブル対応
      *  04.04.00    NACL-竹田　  09/02/25  公害対応
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.05.00    NACL-多々納  09/06/25  薬剤情報提供料・調基判定追加
      *  04.05.00    NACL-多々納  09/11/30  入院回数一括入力対応
      *  04.05.00    NACL-多々納  10/01/15  治験対応
      *  04.05.01    NACL-藤原    11/11/24  病名からの疑いフラグの設定
      *
      *  04.06.01    NACL-藤原    10/06/20  病名の入力順対応
      *  04.06.02    NACL-多々納  10/08/09  複数公費対応
      *  04.06.03    NACL-藤原    10/10/20  難病外来指導管理料対応
      *  04.06.02    NACL-多々納  10/11/19  ４０剤、４０明細対応他
      *  04.06.02    NACL-多々納  11/02/18  用法コメント対応
      *  04.06.04    NACL-藤原    13/05/29  レセ電エラーフラグの判定に
      *                                     主病名フラグを使用しない
      *
      *  04.07.01    NACL-藤原    10/10/20  病名の削除処理対応
      *  04.07.00    NACL-多々納  11/10/13  ＵＩＤ編集対応
      *  04.07.00    NACL-多々納  11/11/02  同日再入院対応
      *  04.07.00    NACL=多々納  12/03/XX  平成２４年４月改正対応
      *  04.07.02    NACL-藤原    12/05/31  病名の入外区分対応
      *  04.07.00    NACL=多々納  12/07/18  API病名のみ登録対応
      *  04.07.03    NACL-藤原    13/09/05  病名の補足コメント対応
      *  04.07.04    NACL-藤原    13/10/08  病名更新時のuserid 設定
      *  04.07.05    NACL-藤原    14/09/26  返却情報の強化対応
      *  04.07.06    NACL-藤原    15/07/29  患者病名登録時の登録時間記録
      *                                     と画面表示対応
      *                                     登録時間を追加
      *
      *  04.08.01    NACL-藤原    16/03/02  入外区分による更新の修正
      *  04.08.02    NACL-藤原    17/03/15  単独病名の病名コードのみ設定時の
      *                                     取り込み修正
      *                                     病名コード数の設定の修正
      *  04.08.03    NACL-藤原    18/08/21  病名からの疑いフラグの設定
      *
      *  05.00.01    NACL-藤原    19/09/10  更新時のエラー追加
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
           03  FLG-PTBYOMEI-UMU    PIC 9(01).
           03  FLG-USERBYOMEI      PIC 9(01).
           03  FLG-SYNONYM         PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
      *
           03  FLG-HOSOKU-CHK      PIC 9(01). 
      *    0:正常 1:エラー  2:単独病名エラー
      *    3:コード順エラー 4:修飾語のみ
      *    5:未コード化傷病名コード
      *    6:単独病名エラー（病名）
      *    9:同一病名重複
           03  FLG-BYOMEI-CHK      PIC 9(01). 
           03  FLG-BYOMEI-DELETE   PIC 9(01). 
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-DABURI-CHK      PIC 9(01).
           03  FLG-DIAG            PIC 9(01).
           03  FLG-HKNCOMBI-DABURI PIC 9(01).
           03  FLG-PTBYOMEI-OK     PIC 9(01).
      *
      *    キー領域
       01  KEY-AREA                    VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-SRYKA     PIC X(02).
           03  KEY-OLD.
               05  KEY-O-SRYKA     PIC X(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-T               PIC 9(04).
      *
           03  IDXX                PIC 9(04).
      *
           03  PTBYO-CNT           PIC 9(02).
           03  PTBYOMEI-CNT        PIC 9(02).
           03  PTBYOMEI-SRYKA-CNT  PIC 9(02).
           03  PTBYOMEI-DOUKI-CNT  PIC 9(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT             PIC 9(04).
           03  CNT-DIAG            PIC 9(04).
           03  CNT-DELETE          PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-HOSPNUM         PIC  9(02).
           03  WRK-HKNCOMBI        PIC  9(04).
           03  WRK-SRYYMD          PIC  X(08).
      *
           03  WRK-PTID            PIC 9(10).
           03  WRK-SRYKA           PIC X(02).
           03  WRK-NYUGAIKBN       PIC X(01).
      *
           03  WRK-BYOYMD.
               05  WRK-BYOYY           PIC X(04).
               05  WRK-BYOMM           PIC X(02).
               05  WRK-BYODD           PIC X(02).
           03  WRK-TENKIYMD.
               05  WRK-TENKIYY         PIC X(04).
               05  WRK-TENKIMM         PIC X(02).
               05  WRK-TENKIDD         PIC X(02).
           03  WRK-TENKI               PIC X(01).
           03  WRK-SYUBYOFLG           PIC X(01).
           03  WRK-UTAGAIFLG           PIC X(01).
           03  WRK-UTAGAIFLG-KETASU    PIC 9(02).
      *
           03  WRK-HOSOKU-TBL. 
               05  WRK-HOSOKU-G    OCCURS  3.
                   07  WRK-THOSOKUCD   PIC X(10).
                   07  WRK-THOSOKU-COMMENT
                                       PIC X(40).
           03  WRK-HOSOKU-COMMENT  PIC X(40).
           03  WRK-HOSOKU-COMMENT1 PIC X(40).
      *
           03  WRK-BYOMEI-TBL. 
               05  WRK-BYOMEICD-G.
                   07  WRK-TBYOMEICD   PIC X(10)   OCCURS  7.
               05  WRK-SPICOD-G.
                   07  WRK-TSPICOD     PIC X(10)   OCCURS  7.
               05  WRK-BYOMEI-G    OCCURS  7.
                   07  WRK-TBYOMEI     PIC X(100).
                   07  WRK-TTANDOKUKBN PIC 9(02).
                   07  WRK-TUTAGAIFLG  PIC X(01).
           03  WRK-BYOMEICD        PIC X(07).
           03  WRK-BYOMEI.
               05  WRK-BYOMEI1     PIC X(80).
               05  WRK-BYOMEI2     PIC X(20). 
           03  WRK-LENGTH          PIC 9(02).
           03  WRK-TANDOKUKBN      PIC 9(01).
           03  WRK-TOKSKNCD        PIC 9(02).
           03  WRK-RECECD-TBL.
               05  WRK-RECECD      PIC X(01)   OCCURS  3.
           03  WRK-MOJISU          PIC 9(03).
      *
           03  WRK-DIAG-AREA.
               05  WRK-DIAG-WARNING
                                   PIC X(03).
               05  WRK-DIAG-CHANGE PIC X(02).
               05  WRK-DIAG-CODE   PIC X(50).
      *
           03  WRK-ERR-BYOMEI      PIC X(80).
           03  WRK-ERR-SRYYMD      PIC X(22).
           03  WRK-ERR-SRYKA       PIC X(20).
      *
           03  WRK-CHK-SRYKA       PIC X(02).
      *
           03  WRK-COL             PIC 9(03).
      *
      *    傷病名編集フラグ（疑い）
           03  WRK-HENFLG1         PIC X(01).
      *    傷病名編集フラグ（急性）
           03  WRK-HENFLG2         PIC X(01). 
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(04).
               05  WRK-WMM         PIC X(02).
               05  WRK-WDD         PIC X(02).
      *                             
           03  WRK-PATHNAME             PIC X(64).
           03  WRK-PTBYO-MCP-PATHNAME   PIC X(64).
      *
       01  WRK-HEN-AREA.
           03  WRK-NENGO           PIC X(04).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *    最小診療科病名情報
       01  WRK-PTBYOMEI-SRYKA-REC.
           COPY    "CPPTBYOMEI.INC"  REPLACING  //PTBYO//
                                     BY         //WRK-PTBYO-SRYKA//.
      *
      *    更新対象病名情報
       01  WRK-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"  REPLACING  //PTBYO//
                                     BY         //WRK-PTBYO//.
      *
      *    診療科別更新対象病名情報
       01  WRK-PTBYOMEI-DOUKI-REC.
           COPY    "CPPTBYOMEI.INC"  REPLACING  //PTBYO//
                                     BY         //WRK-PTBYO-DOUKI//.
      *
      *    病名情報退避
       01  WRK-PTBYOMEI-TAIHI-REC.
           COPY    "CPPTBYOMEI.INC"  REPLACING  //PTBYO//
                                     BY         //WRK-PTBYO-TAIHI//.
      *
      *    病名情報チェック
       01  CHK-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"  REPLACING  //PTBYO//
                                     BY         //CHK-PTBYO//.
      *    同一病名情報
       01  OK-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"  REPLACING  //PTBYO//
                                     BY         //OK-PTBYO//.
      *
      *    転帰レセ電コード
           COPY    "CPMML0016.INC".
      *
       01  WRK-CONS-AREA.
      *    改行コード
           03  WRK-CONS-RETURN     PIC X(01)   VALUE   X"0A".
      *
      *    未コード化傷病名コード
           03  WRK-CONS-BYOMEICD   PIC X(07)   VALUE   "0000999".
      *
      *    の疑い
           03  WRK-CONS-UTAGAI     PIC X(07)   VALUE   "ZZZ8002".                                     
           03  WRK-CONS-UTAGAI-MOJISU
                                   PIC 9(02)   VALUE   6.       
      *    急性
           03  WRK-CONS-KYUSEI-MOJISU
                                   PIC 9(02)   VALUE   4. 
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    ＣＬＡＩＭ接続情報
           COPY    "CPSK9000.INC".
      *
           COPY    "CPSK1005.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    患者病名マスタ−
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    自院病名マスタ
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *    同義語マスタ
       01  SYNONYM-REC.
           COPY    "CPSYNONYM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *    スパ領域
      *    COPY    "COMMON-SPA".
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *    患者番号変換サブ
      *    COPY    "CPORCSPTID.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    拡張漢字チェックサブ
           COPY    "CPORCSKANACHK2.INC".
      *
      *    病名コード検索サブ
           COPY    "CPORCSBYOMEI.INC".
      *
      *    入院サブ
      *    COPY    "CPORCL0031N.INC".
      *
      *    病名組立サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  LF04-REC. 
           COPY    "CPRLF004.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
           COPY    "CPSK1001.INC".
      *
      *    API連携パラメタ領域
           COPY    "CPORCL0031.INC".
      *
      ******************************************************************
       PROCEDURE                       DIVISION
                                       USING   LF04-REC
                                               SPA-AREA
                                               SYS-1001-REC
                                               LNK-ORCL0031AREA.
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
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
      *
           MOVE    SPA-SRYKA           TO  WRK-SRYKA
           MOVE    SPA-PTID            TO  WRK-PTID
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
      *
      *    システム管理検索（ＣＬＡＩＭ接続情報）
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                      SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-9000-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-9000-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-9000-REC
               INITIALIZE                  SYS-9000-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-9000-REC
           ELSE
               INITIALIZE                  SYS-9000-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SYS-1001-HOSPSBT1    =   1
               MOVE    SPACE            TO  SYS-9000-BYOMEI-DOUKI-FLG
                                            SYS-9000-BYOMEI-SYU-FLG
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
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
      *    病名セット処理
           PERFORM 2006-BYOMEI-SET-SEC
      *
           MOVE    CNT-OUT             TO  LNK-OUT-CNT2
      *
           EVALUATE    FLG-DIAG
               WHEN    1
                   MOVE    "02"                TO  LNK-ERR2-CODE
                   MOVE    "警告がある病名が存在します"
                                               TO  LNK-ERR2-MSG
               WHEN    9
                   MOVE    "01"                TO  LNK-ERR2-CODE
                   MOVE    "登録出来ない病名が存在します"
                                               TO  LNK-ERR2-MSG
           END-EVALUATE
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名セット処理
      *****************************************************************
       2006-BYOMEI-SET-SEC                 SECTION.
      *
      *****DISPLAY "病名情報 ================== " 
      *
           MOVE    ZERO            TO  FLG-DIAG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       50
      *
               INITIALIZE                  WRK-BYOMEI-TBL
                                           WRK-HOSOKU-TBL
                                           FLG-BYOMEI-CHK
                                           WRK-TOKSKNCD
                                           WRK-BYOMEI
                                           WRK-HOSOKU-COMMENT
                                           WRK-DIAG-AREA
      *
               IF    ( LF04-SNDSPITBL-SPICOD2 (IDX 1)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPINAME2(IDX 1)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPICOD2 (IDX 2)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPINAME2(IDX 2)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPICOD2 (IDX 3)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPINAME2(IDX 3)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPICOD2 (IDX 4)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPINAME2(IDX 4)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPICOD2 (IDX 5)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPINAME2(IDX 5)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPICOD2 (IDX 6)  =   SPACE )
               AND   ( LF04-SNDSPITBL-SPINAME2(IDX 6)  =   SPACE )
                   IF    ( LF04-SND-SPICOD1 (IDX)  =   SPACE )
                   AND   ( LF04-SND-SPINAME1(IDX)  =   SPACE )
                       CONTINUE
                   ELSE  
      *                連結式病名編集
                       PERFORM 20061-SPICOD1-HENSYU-SEC
                   END-IF
               ELSE
      *            単独式病名編集
                   PERFORM 20061-SPICOD2-HENSYU-SEC
               END-IF
           END-PERFORM    
           .
       2006-BYOMEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    連結式病名編集処理
      *****************************************************************
       20061-SPICOD1-HENSYU-SEC                 SECTION.
      *
           DISPLAY "--------------------------- " 
           DISPLAY "IDX=" IDX 
      *
           DISPLAY " diagnosis code =" LF04-SND-SPICOD1 (IDX)
           DISPLAY " diagnosis name =" LF04-SND-SPINAME1(IDX)
      *
      *    疾患コードが設定されていないとき
           IF         LF04-SND-SPICOD1 (IDX)  =   SPACE
               MOVE    1                      TO  FLG-BYOMEI-CHK
           ELSE  
      *        疾患コードを分割する
               UNSTRING   LF04-SND-SPICOD1 (IDX)  DELIMITED  BY  "."
                                           INTO   WRK-TBYOMEICD (1)
                                                  WRK-TBYOMEICD (2)
                                                  WRK-TBYOMEICD (3)
                                                  WRK-TBYOMEICD (4) 
                                                  WRK-TBYOMEICD (5)
                                                  WRK-TBYOMEICD (6)
                                                  WRK-TBYOMEICD (7)
               END-UNSTRING
           END-IF
      *
      *    ７個以上のコードが設定されているとき
           IF      WRK-TBYOMEICD (7)   NOT =   SPACE
               MOVE    1                   TO  FLG-BYOMEI-CHK
           END-IF
      *
      *    病名コードチェック
           PERFORM 200611-BYOMEICD-CHK-SEC
      *
      *    病名コードがエラー又は未コード化傷病名コードのときは疾患名を編集
           IF      FLG-BYOMEI-CHK      =   1   OR  5
               MOVE    LF04-SND-SPINAME1 (IDX) TO  WRK-BYOMEI
           END-IF 
      *
      *    患者病名マスタ作成
           PERFORM 200612-BYOMEI-HEN-SEC
           .
       20061-SPICOD1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    単独式病名編集処理
      *****************************************************************
       20061-SPICOD2-HENSYU-SEC                 SECTION.
      *
           DISPLAY "--------------------------- " 
           DISPLAY "IDX=" IDX 
      *
      *    病名のみ設定のときコードチェック
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       6
                   OR   (  LF04-SNDSPITBL-SPINAME2(IDX IDY) 
                                               =   SPACE
                       AND LF04-SNDSPITBL-SPICOD2 (IDX IDY)
                                               =   SPACE )
                   OR   (  FLG-BYOMEI-CHK      =   1     )
      *
               DISPLAY " IDY="                     IDY
               DISPLAY " diagnosis dxItem code ="
                                     LF04-SNDSPITBL-SPICOD2 (IDX IDY)
               DISPLAY " diagnosis dxItem name ="
                                     LF04-SNDSPITBL-SPINAME2(IDX IDY)
      *
               IF      LF04-SNDSPITBL-SPICOD2 (IDX IDY)
                                           =   SPACE
                   MOVE    1                   TO  FLG-BYOMEI-CHK 
               ELSE
                   MOVE    LF04-SNDSPITBL-SPICOD2 (IDX IDY)
                                               TO  WRK-TBYOMEICD (IDY)
               END-IF
           END-PERFORM
      *     
      *    病名コードチェック
           PERFORM 200611-BYOMEICD-CHK-SEC
      *
      *    病名コードがエラー又は未コード化傷病名コードのときは疾患名を編集
           IF      FLG-BYOMEI-CHK      =   1   OR  5
               STRING  LF04-SNDSPITBL-SPINAME2(IDX 1) DELIMITED BY SPACE
                       LF04-SNDSPITBL-SPINAME2(IDX 2) DELIMITED BY SPACE
                       LF04-SNDSPITBL-SPINAME2(IDX 3) DELIMITED BY SPACE
                       LF04-SNDSPITBL-SPINAME2(IDX 4) DELIMITED BY SPACE
                       LF04-SNDSPITBL-SPINAME2(IDX 5) DELIMITED BY SPACE
                       LF04-SNDSPITBL-SPINAME2(IDX 6) DELIMITED BY SPACE
                                               INTO    WRK-BYOMEI
               END-STRING
           END-IF
      *
      *    患者病名マスタ作成
           PERFORM 200612-BYOMEI-HEN-SEC
           .
       20061-SPICOD2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コードチェック処理
      *****************************************************************
       200611-BYOMEICD-CHK-SEC             SECTION.
      *
           MOVE    WRK-BYOMEICD-G  TO  WRK-SPICOD-G
      *
      *    補足コメントチェック
           IF    ( LNK-KBN-V2          =   "1" )
           AND   ( LNK-KBN-CODE        =   "1" )
               PERFORM 2006128-HOSOKU-COMMENT-CHK-SEC
      *****    IF      WRK-DIAG-WARNING    NOT =   SPACE
      *****        GO  TO  200611-BYOMEICD-CHK-EXT
      *****    END-IF 
           END-IF
      *
           IF      FLG-BYOMEI-CHK      =   1
               GO  TO  200611-BYOMEICD-CHK-EXT
           END-IF 
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       6
                   OR      WRK-TBYOMEICD (IDY) =   SPACE
                   OR      FLG-BYOMEI-CHK      =   1
      *        
      *        病名コードの半角チェック、長さチェック
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    WRK-TBYOMEICD (IDY) TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-RC2         =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "1"  )
                   EVALUATE    KANACHK-MAX
                       WHEN    4
                           MOVE    SPACE       TO  WRK-BYOMEICD
                           MOVE    "ZZZ"       TO  WRK-BYOMEICD (1:3)
                           MOVE    WRK-TBYOMEICD (IDY)(1:4)
                                               TO  WRK-BYOMEICD (4:4)
                          MOVE    WRK-BYOMEICD
                                               TO  WRK-TBYOMEICD (IDY)
                       WHEN    7
                           CONTINUE
                       WHEN    OTHER
                           MOVE    1           TO  FLG-BYOMEI-CHK
                   END-EVALUATE
               ELSE
                   MOVE    1                   TO  FLG-BYOMEI-CHK
               END-IF
      *
      *        病名マスタ存在チェック
               IF      FLG-BYOMEI-CHK          =   ZERO
                   IF      WRK-TBYOMEICD (IDY)     =   WRK-CONS-BYOMEICD
                       MOVE    5               TO  FLG-BYOMEI-CHK
                   ELSE
                       INITIALIZE                      BYOMEI-REC
                       MOVE    WRK-TBYOMEICD (IDY) TO  BYO-BYOMEICD
                       PERFORM 900-BYOMEI-READ-SEC
                       IF      FLG-BYOMEI          =   1
                           MOVE    1               TO  FLG-BYOMEI-CHK
                       ELSE
                           MOVE    BYO-BYOMEI      TO
                                                   WRK-TBYOMEI    (IDY)
                           MOVE    BYO-TANDOKUKBN  TO
                                                   WRK-TTANDOKUKBN(IDY)
                           MOVE    BYO-UTAGAIFLG   TO
                                                   WRK-TUTAGAIFLG (IDY)
                           IF      WRK-TBYOMEICD (IDY) (1:1)
                                               NOT =   "Z"
                               IF      BYO-NANBYOCD
                                                   NOT =   ZERO
                                   MOVE    BYO-NANBYOCD
                                                       TO  WRK-TOKSKNCD
                               ELSE 
                                   MOVE    BYO-TOKSKNCD
                                                       TO  WRK-TOKSKNCD
                               END-IF
                           END-IF
                       END-IF      
                   END-IF
               END-IF
           END-PERFORM     
      * 
      *    接尾語・接頭語チェック
           IF      FLG-BYOMEI-CHK       =   ZERO
               MOVE    SPACE                   TO  WRK-RECECD-TBL
      * 
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       6
                       OR      WRK-TBYOMEICD (IDY) =   SPACE
                       OR      FLG-BYOMEI-CHK      =   3
                   EVALUATE    WRK-TBYOMEICD (IDY)(1:3)
                                   ALSO    WRK-TBYOMEICD (IDY)(4:1)
                       WHEN    "ZZZ"         ALSO    "1"  THRU    "7"
                           IF    ( WRK-RECECD (2)    NOT =   SPACE )
                           OR    ( WRK-RECECD (3)    NOT =   SPACE ) 
                               MOVE    3             TO  FLG-BYOMEI-CHK
                           ELSE
                               MOVE    "1"           TO  WRK-RECECD (1)
                           END-IF            
                       WHEN    "ZZZ"       ALSO    "8"  
                           IF      WRK-RECECD (2)    =   SPACE 
                               MOVE    3             TO  FLG-BYOMEI-CHK
                           ELSE
                               MOVE    "1"           TO  WRK-RECECD (3)
                           END-IF
                       WHEN    OTHER                 
                           IF    ( WRK-RECECD (3)    NOT =   SPACE ) 
                           OR    ( WRK-RECECD (2)    NOT =   SPACE )
                               MOVE    3             TO  FLG-BYOMEI-CHK
                           ELSE
                               MOVE    "1"           TO  WRK-RECECD (2)
                           END-IF    
                   END-EVALUATE
               END-PERFORM
               IF    ( FLG-BYOMEI-CHK      =   ZERO  )
               AND   ( WRK-RECECD (2)      =   SPACE ) 
                   MOVE    4                   TO  FLG-BYOMEI-CHK
                END-IF
           END-IF 
      *
      *    単独使用禁止の病名チェック（補足コメントエラー以外）
           IF    ( FLG-HOSOKU-CHK      =   2     )
           AND   ( WRK-HOSOKU-COMMENT  =   SPACE )
               CONTINUE
           ELSE
               IF      FLG-BYOMEI-CHK      =   ZERO
                   MOVE    ZERO                TO  WRK-TANDOKUKBN
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       6
                           OR      WRK-TBYOMEICD (IDY) =   SPACE
                       IF      WRK-TTANDOKUKBN (IDY)   =   ZERO
                           MOVE    1               TO  WRK-TANDOKUKBN
                       END-IF
                   END-PERFORM
      *
                   IF      WRK-TANDOKUKBN      =   ZERO
                       IF    ( LNK-KBN-V2          =   "1" )
                       AND   ( LNK-KBN-CODE        =   "1" )
                           IF      WRK-HOSOKU-COMMENT  =   SPACE
                               MOVE    2           TO  FLG-BYOMEI-CHK
                           END-IF
                       ELSE
                           MOVE    2           TO  FLG-BYOMEI-CHK
                       END-IF
                   END-IF
      *
                   IF      FLG-BYOMEI-CHK      =   2
                       MOVE    "W02"               TO  WRK-DIAG-WARNING
                       PERFORM 500-WARNING-HENSYU-SEC
                   END-IF
               END-IF
           END-IF
      *           
      *    マスタから病名を組み立てる
           MOVE    SPACE                   TO  WRK-BYOMEI
           IF      FLG-BYOMEI-CHK          =   2
               STRING  "＊"                DELIMITED   BY  SIZE
                       WRK-TBYOMEI (1)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (2)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (3)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (4)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (5)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (6)     DELIMITED   BY  SPACE
                                           INTO    WRK-BYOMEI
               END-STRING
               INITIALIZE                      WRK-TBYOMEICD (1)
                                               WRK-TBYOMEICD (2)
                                               WRK-TBYOMEICD (3)
                                               WRK-TBYOMEICD (4) 
                                               WRK-TBYOMEICD (5)
                                               WRK-TBYOMEICD (6)
                                               WRK-TBYOMEICD (7)
               MOVE    WRK-CONS-BYOMEICD   TO  WRK-TBYOMEICD (1) 
           ELSE
               STRING  WRK-TBYOMEI (1)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (2)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (3)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (4)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (5)     DELIMITED   BY  SPACE   
                       WRK-TBYOMEI (6)     DELIMITED   BY  SPACE
                                           INTO    WRK-BYOMEI
               END-STRING
           END-IF
           .
       200611-BYOMEICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ作成処理
      *****************************************************************
       200612-BYOMEI-HEN-SEC                 SECTION.
      *
           DISPLAY " diagnosis connectname ="  WRK-BYOMEI
           DISPLAY " diagnosis check flag  ="  FLG-BYOMEI-CHK
      *
      *    病名が空白のとき
           IF      WRK-BYOMEI          =   SPACE
               MOVE    "E03"               TO  WRK-DIAG-WARNING
               PERFORM 500-WARNING-HENSYU-SEC
               GO  TO  200612-BYOMEI-HEN-EXT
           END-IF
      *
      *    補足コメントコードがエラーのとき
           IF    ( LNK-KBN-V2          =   "1" )
           AND   ( LNK-KBN-CODE        =   "1" )
               IF    ( FLG-HOSOKU-CHK      =   2     )
               AND   ( WRK-HOSOKU-COMMENT  =   SPACE )
                   MOVE    "E04"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
                   GO  TO  200612-BYOMEI-HEN-EXT
               END-IF 
           END-IF
      *
      *    病名コードがエラー又は未コード化傷病名コードのとき
      *    疾患名から単独使用禁止の病名チェック
           IF      FLG-BYOMEI-CHK      =   1   OR  5
               INITIALIZE                  ORCSBYOMEIAREA
               MOVE    "1"                 TO  LNK-BYO-SYORI
               MOVE    LF04-SND-SKNSTARTDATE(IDX)(1:4)
                                           TO  LNK-BYO-SRYYMD-IN (1:4)
               MOVE    LF04-SND-SKNSTARTDATE(IDX)(6:2)
                                           TO  LNK-BYO-SRYYMD-IN (5:2)
               MOVE    LF04-SND-SKNSTARTDATE(IDX)(9:2)
                                           TO  LNK-BYO-SRYYMD-IN (7:2)
               MOVE    WRK-BYOMEI          TO  LNK-BYO-BYOMEI
               CALL    "ORCSBYOMEI"        USING    ORCSBYOMEIAREA
                                                    SPA-AREA
               IF      LNK-BYO-TANDOKUKBN-ERR  =   1
                   IF    ( LNK-KBN-V2          =   "1" )
                   AND   ( LNK-KBN-CODE        =   "1" )
                       IF      WRK-HOSOKU-COMMENT  =   SPACE
                           MOVE    6               TO  FLG-BYOMEI-CHK
                       END-IF
                   ELSE
                       MOVE    6                   TO  FLG-BYOMEI-CHK
                   END-IF
               END-IF
      *
               IF      FLG-BYOMEI-CHK          =   6
                   MOVE    "W02"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
      *
                   STRING  "＊"                DELIMITED   BY  SIZE
                           LNK-BYO-BYOMEI      DELIMITED   BY  SPACE
                                               INTO    WRK-BYOMEI
                   END-STRING
      *
                   DISPLAY " diagnosis connectname ="  WRK-BYOMEI
                   DISPLAY " diagnosis check flag  ="  FLG-BYOMEI-CHK
               END-IF
           END-IF
      *
      *    病名チェック
           MOVE    SPACE               TO  WRK-DIAG-AREA
      * 
           IF      WRK-BYOMEI      NOT =   SPACE
      *        全角チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI 
               MOVE    WRK-BYOMEI          TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "2"  )
                   MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                               TO  WRK-BYOMEI
               ELSE
                   MOVE    "W03"               TO  WRK-DIAG-WARNING
               END-IF
      *
               IF  WRK-DIAG-WARNING        =   SPACE
      *            改行コードチェック
                   MOVE    ZERO                TO  WRK-COL
                   INSPECT WRK-BYOMEI          TALLYING    WRK-COL
                                               FOR ALL WRK-CONS-RETURN
                   IF      WRK-COL             >   ZERO
                       MOVE    "W04"               TO  WRK-DIAG-WARNING
                   END-IF
               END-IF
      *
               IF  WRK-DIAG-WARNING    NOT =   SPACE
                   MOVE    "？？？？？"        TO  WRK-BYOMEI
                   PERFORM 500-WARNING-HENSYU-SEC
               END-IF
           END-IF
      *
      *    疾患開始日
           DISPLAY "startDate ="      LF04-SND-SKNSTARTDATE(IDX)
      *
           MOVE    LF04-SND-SKNSTARTDATE(IDX)(1:4)
                                       TO  WRK-BYOYY
           MOVE    LF04-SND-SKNSTARTDATE(IDX)(6:2)
                                       TO  WRK-BYOMM
           MOVE    LF04-SND-SKNSTARTDATE(IDX)(9:2)
                                       TO  WRK-BYODD
      *    疾患終了日
           DISPLAY "endDate   ="       LF04-SND-SKNENDDATE(IDX)
      *
           MOVE    LF04-SND-SKNENDDATE(IDX)(1:4)
                                       TO  WRK-TENKIYY
           MOVE    LF04-SND-SKNENDDATE(IDX)(6:2)
                                       TO  WRK-TENKIMM
           MOVE    LF04-SND-SKNENDDATE(IDX)(9:2)
                                       TO  WRK-TENKIDD
      *    転帰
           DISPLAY "outcome ="        LF04-SND-TENKI(IDX)
      *
           MOVE    SPACE               TO  WRK-TENKI
           IF      LF04-SND-TENKI  (IDX)   =   "delete"
               MOVE    1                   TO  FLG-BYOMEI-DELETE
           ELSE
               MOVE    0                   TO  FLG-BYOMEI-DELETE
               IF      LF04-SND-SKNENDDATE(IDX)    NOT =   SPACE
      *            転帰が空白の時、治癒
                   IF      LF04-SND-TENKI  (IDX)       =   SPACE 
                       MOVE    "1"              TO  WRK-TENKI
                   ELSE
                       PERFORM 2006121-TENKSET-SEC
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  WRK-SYUBYOFLG
                                           WRK-UTAGAIFLG
                                           WRK-NYUGAIKBN
      *    疑いフラグ・主病名フラグ
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       4
               DISPLAY " IDY " IDY
               DISPLAY " categories id   ="
                                       LF04-SNDBNRTBL-BNRTBL1 (IDX IDY)
               DISPLAY " categories name ="
                                       LF04-SNDBNRTBL-BNRNAME1(IDX IDY)
      *
               IF    ( LF04-SNDBNRTBL-BNRTBL1 (IDX IDY)
                                       =   "MML0012"       )
               AND   ( LF04-SNDBNRTBL-BNRNAME1 (IDX IDY)
                                       =   "mainDiagnosis" )
                   MOVE    "1"             TO WRK-SYUBYOFLG
               END-IF
               IF    ( LF04-SNDBNRTBL-BNRTBL1 (IDX IDY)
                                       =   "MML0015"            )
               AND   ( LF04-SNDBNRTBL-BNRNAME1 (IDX IDY)
                                       =   "suspectedDiagnosis" )
                   MOVE    "1"             TO  WRK-UTAGAIFLG
               END-IF
           END-PERFORM
      *
      *    入外区分
           DISPLAY " "
           DISPLAY " InOutPatient ="   LF04-SND-NYUGAIKBN (IDX)
      *
           EVALUATE    LF04-SND-NYUGAIKBN (IDX)
               WHEN    "inpatient"
                   MOVE    "1"         TO  WRK-NYUGAIKBN
               WHEN    "outpatient"
                   MOVE    "2"         TO  WRK-NYUGAIKBN
               WHEN    OTHER
                   MOVE    SPACE       TO  WRK-NYUGAIKBN
           END-EVALUATE
      *
      *    更新処理
           INITIALIZE                  FLG-PTBYOMEI-UMU
                                       WRK-PTBYOMEI-REC
                                       WRK-PTBYOMEI-SRYKA-REC
                                       WRK-PTBYOMEI-DOUKI-REC
                                       WRK-UTAGAIFLG-KETASU
                                       PTBYOMEI-CNT
                                       PTBYOMEI-SRYKA-CNT
                                       PTBYOMEI-DOUKI-CNT
      *
      *    「の疑い」を検索
           PERFORM VARYING IDY FROM    1  BY  2
                   UNTIL   IDY >       75
               IF      WRK-BYOMEI1 (IDY:6)  =   "の疑い"
                   MOVE    IDY              TO  WRK-UTAGAIFLG-KETASU
                   MOVE    80               TO  IDY
               END-IF
           END-PERFORM
      *
           IF      FLG-BYOMEI-DELETE   =   1
      *       「の疑い」を検索したとき
               IF      WRK-UTAGAIFLG-KETASU    >   ZERO
                   EVALUATE    WRK-UTAGAIFLG
                       WHEN    SPACE
                       WHEN    "1"
                           MOVE    "1"         TO  WRK-UTAGAIFLG
                       WHEN    "2"
                       WHEN    "3"
                           MOVE    "3"         TO  WRK-UTAGAIFLG
                   END-EVALUATE
               END-IF    
      *
               IF      WRK-UTAGAIFLG       =   "3" OR  "2"
                   CONTINUE
               ELSE
      *            「急性」を検索
                   PERFORM VARYING IDY FROM    1  BY  2
                           UNTIL   IDY >       77
                       IF      WRK-BYOMEI1 (IDY:4)  =   "急性"
                           EVALUATE    WRK-UTAGAIFLG
                               WHEN    SPACE
                                   MOVE    "2"         TO  WRK-UTAGAIFLG
                               WHEN    "1"
                                   MOVE    "3"         TO  WRK-UTAGAIFLG
                           END-EVALUATE
                           MOVE    80               TO  IDY
                       END-IF     
                   END-PERFORM
               END-IF     
           END-IF
      *
      *    同一患者病名チェック処理
           PERFORM 2008-BYOMEI-CHK-SEC
      *??
           DISPLAY "FLG-PTBYOMEI-OK="  FLG-PTBYOMEI-OK
      *??
      *
      *    マスタとの重複チェック
           INITIALIZE                  CHK-PTBYOMEI-REC
           IF      FLG-PTBYOMEI-OK     =   1
               MOVE    OK-PTBYOMEI-REC     TO  CHK-PTBYOMEI-REC
           END-IF
           PERFORM 2009-BYOMEI-DABURI-CHK-SEC
           IF      FLG-BYOMEI-CHK      =   9
               GO  TO  200612-BYOMEI-HEN-EXT
           END-IF
      *
      *    同一病名が存在するとき
           IF      FLG-PTBYOMEI-OK     =   1
               MOVE    OK-PTBYOMEI-REC     TO  PTBYOMEI-REC
               PERFORM 2006124-KOUSIN-02-SEC
      *        同期フラグ
               IF      SYS-9000-BYOMEI-DOUKI-FLG
                                           =   "1"
                   PERFORM 2006125-KOUSIN-03-SEC
               END-IF
               GO  TO  200612-BYOMEI-HEN-EXT
           END-IF
      *
      *    患者病名マスタ検索（同一開始日・病名）
      *
           MOVE    SPACE               TO  WRK-ERR-SRYYMD
                                           WRK-ERR-SRYKA
           INITIALIZE                      PTBYOMEI-SRYKA-CNT
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
           MOVE    WRK-HOSOKU-COMMENT  TO  PTBYO-HOSOKU-COMMENT
           IF      FLG-BYOMEI-DELETE   =   1 
               MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
                                               PTBYO-XXBYOMEI
           ELSE
               IF      WRK-UTAGAIFLG-KETASU    =   ZERO
                   MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
                   STRING  WRK-BYOMEI1 (1:80)  DELIMITED BY  SPACE
                           "の疑い"            DELIMITED BY  SIZE  
                                               INTO    PTBYO-XXBYOMEI
                   END-STRING
               ELSE
                   IF      WRK-UTAGAIFLG-KETASU    =   1
                       MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
                                                       PTBYO-XXBYOMEI
                   ELSE
                       MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
                       STRING  WRK-BYOMEI1 (1:WRK-UTAGAIFLG-KETASU - 1)
                                                   DELIMITED BY  SIZE
                               WRK-BYOMEI1 (WRK-UTAGAIFLG-KETASU + 6:)
                                                   DELIMITED BY  SPACE
                                               INTO    PTBYO-XXBYOMEI
                       END-STRING
                   END-IF
               END-IF
           END-IF    
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           IF    ( LNK-KBN-V2          =   "1" )
           AND   ( LNK-KBN-CODE        =   "1" )
               MOVE    "key47"             TO  WRK-PTBYO-MCP-PATHNAME
           ELSE
               MOVE    "key29"             TO  WRK-PTBYO-MCP-PATHNAME
           END-IF
           MOVE    WRK-PTBYO-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-PTBYO-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
           PERFORM             UNTIL   FLG-PTBYOMEI        =   1
      *        最小診療科の病名情報を取得
               IF      WRK-PTBYO-SRYKA     =   SPACE
                   MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-REC
               END-IF
      *        最小診療科で同一病名があれば病名情報を取得
               IF    ( PTBYO-SRYKA         =   WRK-PTBYO-SRYKA    )
               AND   ( PTBYO-BYOMEI        =   WRK-BYOMEI1 (1:80) )
                   MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-REC
               END-IF
      *        同一診療科の病名情報を取得
               IF      PTBYO-SRYKA         =   WRK-SRYKA
                   ADD     1               TO  PTBYOMEI-SRYKA-CNT
                   IF      FLG-PTBYOMEI-UMU    >   2
                       CONTINUE
                   ELSE 
                       MOVE    PTBYOMEI-REC    TO
                                               WRK-PTBYOMEI-SRYKA-REC
                       IF      FLG-PTBYOMEI-UMU    =   ZERO
                           MOVE    1               TO  FLG-PTBYOMEI-UMU
                       END-IF
                       IF      PTBYO-BYOMEI        =   WRK-BYOMEI1(1:80)
                           IF      FLG-PTBYOMEI-UMU    =   2
                               MOVE    4           TO  FLG-PTBYOMEI-UMU
                           ELSE
                               MOVE    3           TO  FLG-PTBYOMEI-UMU
                           END-IF
                       END-IF
                   END-IF
               ELSE
                   MOVE    2                   TO  FLG-PTBYOMEI-UMU
               END-IF
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-PTBYO-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-PTBYO-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    同一診療科に病名が複数のときは更新しない
           IF      PTBYOMEI-SRYKA-CNT  >   1
               MOVE    "E05"               TO  WRK-DIAG-WARNING
               MOVE    WRK-SRYKA           TO  PTBYO-SRYKA
               PERFORM 5001-SRYKA-HENSYU-SEC
               MOVE    SYS-1005-SRYKANAME1
                                           TO  WRK-ERR-SRYKA
               PERFORM 500-WARNING-HENSYU-SEC
               GO  TO  200612-BYOMEI-HEN-EXT
           END-IF
      *
           IF      FLG-PTBYOMEI-UMU     =   ZERO
      *        同一開始日・病名で存在しないとき
               PERFORM 2006123-KOUSIN-01-SEC
           ELSE
               IF    ( SYS-9000-BYOMEI-SYU-FLG
                                            =   SPACE )
               AND   ( SYS-9000-BYOMEI-DOUKI-FLG
                                            =   SPACE )
      *            同期・集約フラグの設定がないとき
                   IF      WRK-PTBYO-SRYKA-RENNUM   =   ZERO
                       PERFORM 2006123-KOUSIN-01-SEC
                   ELSE
                       MOVE    WRK-PTBYOMEI-SRYKA-REC   TO  PTBYOMEI-REC
                       PERFORM 2006124-KOUSIN-02-SEC
                   END-IF
               ELSE
      *            集約フラグ
                   IF      SYS-9000-BYOMEI-SYU-FLG
                                               =   SPACE
                       IF      WRK-PTBYO-SRYKA-RENNUM  =   ZERO
                           PERFORM 2006123-KOUSIN-01-SEC
                       END-IF
                   ELSE
                       IF      WRK-PTBYO-SRYKA-RENNUM  =   ZERO
                           MOVE    WRK-PTBYOMEI-REC    TO  PTBYOMEI-REC
                       ELSE
                           MOVE    WRK-PTBYOMEI-SRYKA-REC
                                                       TO  PTBYOMEI-REC
                       END-IF
                       PERFORM 2006124-KOUSIN-02-SEC
                   END-IF
      *            同期フラグ
                   IF      SYS-9000-BYOMEI-DOUKI-FLG
                                               =   "1"
      *****************IF      FLG-PTBYOMEI-UMU    =   2
                           PERFORM 2006125-KOUSIN-03-SEC
      *****************END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-BYOMEI-DELETE   =   1
               IF      CNT-DELETE          =   0
                   MOVE    "E06"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
               END-IF
               GO  TO  200612-BYOMEI-HEN-EXT
           END-IF
      *
           IF      FLG-PTBYOMEI-OK     =   1 
               GO  TO  200612-BYOMEI-HEN-EXT
           END-IF
      *
      *    入外区分の更新
           INITIALIZE                  FLG-PTBYOMEI-UMU
                                       WRK-PTBYOMEI-REC
                                       WRK-PTBYOMEI-SRYKA-REC
                                       WRK-PTBYOMEI-DOUKI-REC
                                       PTBYOMEI-CNT
                                       PTBYOMEI-SRYKA-CNT
                                       PTBYOMEI-DOUKI-CNT
      *    患者病名マスタ検索（同一開始日・病名）
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
           MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
                                           PTBYO-XXBYOMEI
           MOVE    WRK-HOSOKU-COMMENT  TO  PTBYO-HOSOKU-COMMENT
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           IF    ( LNK-KBN-V2          =   "1" )
           AND   ( LNK-KBN-CODE        =   "1" )
               MOVE    "key47"             TO  WRK-PTBYO-MCP-PATHNAME
           ELSE
               MOVE    "key29"             TO  WRK-PTBYO-MCP-PATHNAME
           END-IF
           MOVE    WRK-PTBYO-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-PTBYO-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
           PERFORM             UNTIL   FLG-PTBYOMEI        =   1
      *        最小診療科の病名情報を取得
               IF      WRK-PTBYO-SRYKA     =   SPACE
                   MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-REC
               END-IF
      *        最小診療科で同一病名の件数取得
               IF      PTBYO-SRYKA         =   WRK-PTBYO-SRYKA
                   ADD     1                   TO  PTBYOMEI-CNT
               END-IF
      *        同一診療科の病名情報を取得
               IF      PTBYO-SRYKA         =   WRK-SRYKA
                   ADD     1               TO  PTBYOMEI-SRYKA-CNT
                   IF      FLG-PTBYOMEI-UMU    >   2
                       CONTINUE
                   ELSE 
                       MOVE    PTBYOMEI-REC    TO
                                               WRK-PTBYOMEI-SRYKA-REC
                       IF      FLG-PTBYOMEI-UMU    =   ZERO
                           MOVE    1               TO  FLG-PTBYOMEI-UMU
                       END-IF
                       IF      PTBYO-BYOMEI        =   WRK-BYOMEI1(1:80)
                           IF      FLG-PTBYOMEI-UMU    =   2
                               MOVE    4           TO  FLG-PTBYOMEI-UMU
                           ELSE
                               MOVE    3           TO  FLG-PTBYOMEI-UMU
                           END-IF
                       END-IF
                   END-IF
               ELSE
                   MOVE    2                   TO  FLG-PTBYOMEI-UMU
               END-IF
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-PTBYO-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-PTBYO-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTBYOMEI-UMU     =   ZERO
      *        同一開始日・病名で存在しないとき
               GO  TO  200612-BYOMEI-HEN-EXT
           ELSE
               IF    ( SYS-9000-BYOMEI-SYU-FLG
                                            =   SPACE )
               AND   ( SYS-9000-BYOMEI-DOUKI-FLG
                                            =   SPACE )
      *            同期・集約フラグの設定がないとき
                   IF      WRK-PTBYO-SRYKA-RENNUM   >   ZERO
                       MOVE    WRK-PTBYOMEI-SRYKA-REC   TO  PTBYOMEI-REC
                       MOVE    PTBYOMEI-SRYKA-CNT       TO  PTBYO-CNT
                       PERFORM 2006126-KOUSIN-04-SEC
                   END-IF
               ELSE
      *            集約フラグ
                   IF      SYS-9000-BYOMEI-SYU-FLG
                                               =   "1"
                       IF      WRK-PTBYO-SRYKA-SRYKA   =   SPACE
                           MOVE    WRK-PTBYOMEI-REC    TO  PTBYOMEI-REC
                           MOVE    PTBYOMEI-CNT        TO  PTBYO-CNT
                       ELSE
                           MOVE    WRK-PTBYOMEI-SRYKA-REC
                                                       TO  PTBYOMEI-REC
                           MOVE    PTBYOMEI-SRYKA-CNT  TO  PTBYO-CNT
                       END-IF
                       PERFORM 2006126-KOUSIN-04-SEC
                   END-IF
      *            同期フラグ
                   IF      SYS-9000-BYOMEI-DOUKI-FLG
                                               =   "1"
                       PERFORM 2006127-KOUSIN-05-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       200612-BYOMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    転帰編集処理
      *****************************************************************
       2006121-TENKSET-SEC                 SECTION.
      *
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL       IDX-T   >   14
      *        転帰が空白の時、治癒
               IF      LF04-SND-TENKI (IDX)    =   MML0016-TENKI (IDX-T)
                   MOVE    MML0016-ORCA-TENKI (IDX-T)
                                               TO  WRK-TENKI
                   MOVE    14                  TO  IDX-T
               END-IF
           END-PERFORM
      *
      *    該当の転帰が無い場合は治癒にする
           IF      WRK-TENKI           =   SPACE
               MOVE    "1"              TO  WRK-TENKI
           END-IF
      *
           .
       2006121-TENKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ追加処理
      *****************************************************************
       2006123-KOUSIN-01-SEC           SECTION.
      *
           IF      FLG-BYOMEI-DELETE   =   1
               GO  TO  2006123-KOUSIN-01-EXT
           END-IF
      *
      *    連番取得
           MOVE    ZERO                TO  WRK-PTBYO-RENNUM
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-SRYKA           TO  PTBYO-SRYKA
           MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "key7"              TO  WRK-PATHNAME
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   ZERO
               IF      PTBYO-RENNUM        =   99
                   MOVE    "14"                TO  LNK-ERR2-CODE
                   MOVE
                   "全ての病名の更新に失敗しました（上限数超過）"
                                           TO  LNK-ERR2-MSG 
                   GO  TO  2006123-KOUSIN-01-EXT
               ELSE
                   MOVE    PTBYO-RENNUM        TO  WRK-PTBYO-RENNUM
               END-IF
           END-IF
           ADD     1                   TO  WRK-PTBYO-RENNUM
      *    病名追加
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-SRYKA           TO  PTBYO-SRYKA
           MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
           MOVE    WRK-PTBYO-RENNUM    TO  PTBYO-RENNUM
           PERFORM 20061231-PTBYOMEI-HENSYU-SEC
           IF      WRK-TENKIYMD    NOT =   SPACE
      *        転帰日
               MOVE    WRK-TENKIYMD        TO  PTBYO-TENKIYMD
      *        転帰区分
               MOVE    WRK-TENKI           TO  PTBYO-TENKIKBN
           END-IF
      *
           MOVE    9999                TO  PTBYO-SEQNUM
           MOVE    SMCNDATE-YMD        TO  PTBYO-CREYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-CREHMS
           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
      *
           IF      LNK-KBN-CODE        =   "1"
               MOVE    "api"               TO  PTBYO-OPID
           ELSE
               MOVE    "claim"             TO  PTBYO-OPID
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               DISPLAY "INS PTBYOMEI-KEY =>" PTBYO-KEY  "."
               MOVE    "11"                TO  LNK-ERR2-CODE
               MOVE
                   "全ての病名の更新に失敗しました（追加時にエラー）"
                                           TO  LNK-ERR2-MSG 
           ELSE
               ADD     1                   TO  CNT-OUT
      *
               MOVE    SPACE               TO  WRK-DIAG-CHANGE
               IF      PTBYO-KHNBYOMEICD   =   WRK-CONS-BYOMEICD
                   INITIALIZE                  SYNONYM-REC
                   MOVE    PTBYO-BYOMEI        TO  SYNONYM-BYOMEI
                   PERFORM 5001-SYNONYM-CHK-SEC
                   IF      FLG-SYNONYM         =   ZERO
                       MOVE    "03"                TO  WRK-DIAG-CHANGE
                   END-IF
               ELSE
                   IF      PTBYO-BYOMEIHENFLG  =   SPACE
                       INITIALIZE                  BYOMEI-REC
                       MOVE    PTBYO-KHNBYOMEICD   TO  BYO-BYOMEICD
                       PERFORM 900-BYOMEI-READ-SEC
                       IF      BYO-IKOSAKICD   NOT =   SPACE
                           MOVE    "01"            TO  WRK-DIAG-CHANGE
                       ELSE    
                           IF      BYO-HAISIYMD    =   ALL "9" OR  SPACE
                              CONTINUE
                           ELSE
                               MOVE    "02"        TO  WRK-DIAG-CHANGE
                               INITIALIZE              SYNONYM-REC
                               MOVE    PTBYO-BYOMEI
                                                   TO  SYNONYM-BYOMEI
                               PERFORM 5001-SYNONYM-CHK-SEC
                               IF      FLG-SYNONYM =   ZERO
                                   MOVE    "03"    TO  WRK-DIAG-CHANGE
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF      
               IF     WRK-DIAG-CHANGE  NOT =   SPACE
                   MOVE    "W01"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
               END-IF
           END-IF
           .
       2006123-KOUSIN-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       20061231-PTBYOMEI-HENSYU-SEC                 SECTION.
      *
      *    疑いフラグ・主病名フラグ
           MOVE    WRK-UTAGAIFLG   TO  PTBYO-UTAGAIFLG
           MOVE    WRK-SYUBYOFLG   TO  PTBYO-SYUBYOFLG
      *
           IF    ( LNK-KBN-V2          =   "1" )
           AND   ( LNK-KBN-CODE        =   "1" )
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       3
                       OR      WRK-THOSOKUCD (IDY) =   SPACE
      *           補足コメントコード
                   MOVE    WRK-THOSOKUCD (IDY) 
                                           TO  PTBYO-HOSOKUCD (IDY)
               END-PERFORM
      *        補足コメント
               MOVE    WRK-HOSOKU-COMMENT 
                                       TO  PTBYO-HOSOKU-COMMENT
           END-IF
      *
      *    疾患コードがエラー又は未コード化傷病名コード以外のとき
           IF      FLG-BYOMEI-CHK      NOT =   1   AND 5   AND 6
               MOVE    ZERO                TO  IDZ
                                               PTBYO-BYOMEICDSU
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       6
                       OR      WRK-TBYOMEICD (IDY) =   SPACE
      *           病名コード
                   MOVE    WRK-TBYOMEICD (IDY) 
                                           TO  PTBYO-BYOMEICD (IDY)
      *            疑いフラグ（病名コードより）
                   IF      PTBYO-UTAGAIFLG     =   "3"
                       CONTINUE
                   ELSE
                       EVALUATE    PTBYO-UTAGAIFLG ALSO
                                               WRK-TUTAGAIFLG (IDY)
                           WHEN    SPACE           ALSO    "1"
                           WHEN    SPACE           ALSO    "2"
                               MOVE    WRK-TUTAGAIFLG (IDY)
                                                   TO  PTBYO-UTAGAIFLG
                           WHEN    "2"             ALSO    "1"
                           WHEN    "1"             ALSO    "2"
                               MOVE    "3"         TO  PTBYO-UTAGAIFLG
                       END-EVALUATE
                   END-IF      
      *
      *************IF      FLG-BYOMEI-CHK      =   ZERO
      *                病名コード数
                       ADD     1                   TO  PTBYO-BYOMEICDSU
      *                基本病名コード・基本部位コード
                       IF      PTBYO-BYOMEICD (IDY)(1:1)
                                               NOT =   "Z" 
                           MOVE    PTBYO-BYOMEICD (IDY)
                                               TO  PTBYO-KHNBYOMEICD
                       ELSE
                           IF      PTBYO-BYOMEICD (IDY) (1:4)
                                                   =   "ZZZ1"
                               ADD     1       TO  IDZ
                               IF      IDZ         <   4
                                   MOVE    PTBYO-BYOMEICD (IDY)
                                               TO  PTBYO-KHNBUICD(IDZ)
                               END-IF
                           END-IF
                       END-IF
      *************END-IF  
               END-PERFORM
      *
      *        病名
      *****    MOVE    LF04-SND-SPINAME1 (IDX)    TO  PTBYO-BYOMEI
               MOVE    WRK-BYOMEI1 (1:80)         TO  PTBYO-BYOMEI
      *
      *        病名文字数
               PERFORM VARYING     IDY     FROM    80 BY  -1
                       UNTIL       IDY     <   1
                   IF      PTBYO-BYOMEI(IDY:1)     NOT =   SPACE
                       COMPUTE PTBYO-BYOMEIMOJI    =   IDY /   2
                       MOVE    1                   TO  IDY
                   END-IF
               END-PERFORM
      *
      *        慢性区分
               MOVE    WRK-TOKSKNCD        TO  PTBYO-MANSEIKBN
      *
      *        病名編集フラグ（コードがエラーのとき、病名が長いとき）
               IF    ( FLG-BYOMEI-CHK  NOT =   ZERO  )
               OR    ( WRK-BYOMEI2     NOT =   SPACE )    
                   MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG
               END-IF
      *
      *        レセ電エラーフラグ（編集病名のとき）
               IF      PTBYO-BYOMEIHENFLG          =   "1"
                   IF      PTBYO-BYOMEIMOJI        >   20
                       MOVE    "1" TO  PTBYO-RECEDENFLG       
                   END-IF
               END-IF
      *
      *        レセ電エラーフラグ（疑いフラグが設定されているとき）
               IF    ( PTBYO-RECEDENFLG        =   SPACE ) 
               AND   ( PTBYO-UTAGAIFLG     NOT =   SPACE )
                   INITIALIZE                      ORCSMKBYOMEIAREA
                   MOVE    PTBYO-UTAGAIFLG     TO  SMKBYO-UTAGAIFLG
                   MOVE    PTBYO-BYOMEI        TO  SMKBYO-BYOMEI
                   MOVE    PTBYO-BYOMEIHENFLG  TO  SMKBYO-BYOMEIHENFLG
                   MOVE    PTBYO-BYOMEICD-G    TO  SMKBYO-BYOMEICD-G
                   CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
      *
                   MOVE    SMKBYO-BYOMEIHENFLG TO  PTBYO-BYOMEIHENFLG
      *
                   COMPUTE WRK-MOJISU      =   SMKBYO-LENGTH  /   2
                   IF      PTBYO-BYOMEIHENFLG    =   "1"
                       IF      WRK-MOJISU  >   20
                           MOVE    "1"     TO  PTBYO-RECEDENFLG
                       END-IF
                   END-IF    
               END-IF    
           ELSE
      *        疾患コードがエラー又は未コード化傷病名コードのとき
      *        病名からコード検索
               INITIALIZE                  ORCSBYOMEIAREA
               MOVE    "1"                 TO  LNK-BYO-SYORI
               MOVE    PTBYO-SRYYMD        TO  LNK-BYO-SRYYMD-IN
               MOVE    PTBYO-UTAGAIFLG     TO  LNK-BYO-UTAGAIFLG-IN
               MOVE    WRK-BYOMEI1         TO  LNK-BYO-BYOMEI
               MOVE    PTBYO-SYUBYOFLG     TO  LNK-BYO-SYUBYOFLG 
               CALL    "ORCSBYOMEI"        USING    ORCSBYOMEIAREA
                                                    SPA-AREA
      *  
      *        疑いフラグ
               IF      PTBYO-UTAGAIFLG     =   "3"
                   CONTINUE
               ELSE
                   EVALUATE    PTBYO-UTAGAIFLG ALSO
                                           LNK-BYO-UTAGAIFLG
                       WHEN    ANY             ALSO    "3"
                           MOVE    "3"         TO  PTBYO-UTAGAIFLG
                       WHEN    SPACE           ALSO    "1"
                       WHEN    SPACE           ALSO    "2"
                           MOVE    LNK-BYO-UTAGAIFLG
                                                   TO  PTBYO-UTAGAIFLG
                       WHEN    "2"             ALSO    "1"
                       WHEN    "1"             ALSO    "2"
                       WHEN    ANY             ALSO    "3"
                           MOVE    "3"         TO  PTBYO-UTAGAIFLG
                   END-EVALUATE
               END-IF      
      *        慢性区分
               IF      LNK-BYO-NANBYOCD
                                   NOT =   ZERO
                   MOVE    LNK-BYO-NANBYOCD    TO  PTBYO-MANSEIKBN
               ELSE
                   IF      LNK-BYO-MANSEIKBN   NOT =   ZERO
                       MOVE    LNK-BYO-MANSEIKBN   TO  PTBYO-MANSEIKBN
                   END-IF
               END-IF
      *        病名コード
               PERFORM VARYING IDX4    FROM    1   BY  1
                       UNTIL   IDX4    >       LNK-BYO-BYOMEI-MAX
                   MOVE    LNK-BYO-TBYOMEICD (IDX4)
                                       TO  PTBYO-BYOMEICD  (IDX4)
               END-PERFORM
      *        病名コード数
               MOVE    LNK-BYO-BYOMEICDSU  TO  PTBYO-BYOMEICDSU
      *        基本病名コード
               MOVE    LNK-BYO-KHNBYOMEICD TO  PTBYO-KHNBYOMEICD
      *        基本部位コード
               MOVE    LNK-BYO-KHNBUICD (1)
                                           TO  PTBYO-KHNBUICD (1)
               MOVE    LNK-BYO-KHNBUICD (2)
                                           TO  PTBYO-KHNBUICD (2)
               MOVE    LNK-BYO-KHNBUICD (3)
                                           TO  PTBYO-KHNBUICD (3)
      *        レセ電エラーフラグ
               MOVE    LNK-BYO-RECEDENFLG  TO  PTBYO-RECEDENFLG
      *        病名文字数
               MOVE    LNK-BYO-BYOMEIMOJI  TO  PTBYO-BYOMEIMOJI
      *        病名
               MOVE    LNK-BYO-BYOMEI      TO  PTBYO-BYOMEI
               IF      LNK-BYO-TANDOKUKBN-ERR  =   1
      *            単独病名のとき
                   IF    ( LNK-KBN-V2              =   "1"   )
                   AND   ( LNK-KBN-CODE            =   "1"   )
                   AND   ( PTBYO-HOSOKU-COMMENT
                                               NOT =   SPACE )
                       CONTINUE
                   ELSE
                       STRING  "＊"            DELIMITED   BY  SIZE
                               LNK-BYO-BYOMEI  DELIMITED   BY  SPACE
                                               INTO    PTBYO-BYOMEI
                       END-STRING
                       COMPUTE PTBYO-BYOMEIMOJI    =   PTBYO-BYOMEIMOJI
                                                       +   2
                   END-IF
               END-IF
      *        病名編集フラグ
               IF    ( LNK-BYO-BYOMEICD-ERR    =   1  )
               OR    ( LNK-BYO-TANDOKUKBN-ERR  =   1  )
               OR    ( LNK-BYO-BYOMEIMOJI      >   80 )
      *            単独病名・修飾語コードのみ・病名が長いとき
                   MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG
               ELSE
                   MOVE    LNK-BYO-BYOMEIHENFLG
                                               TO  PTBYO-BYOMEIHENFLG
               END-IF
           END-IF
      *
           .
       20061231-PTBYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ更新処理
      *****************************************************************
       2006124-KOUSIN-02-SEC           SECTION.
      *
           IF      FLG-BYOMEI-DELETE   =   1
               MOVE    "1"                 TO  PTBYO-DLTFLG
               IF      LNK-KBN-CODE        =   "1"
                   MOVE    "api"               TO  PTBYO-DLT-OPID
               ELSE
                   MOVE    "claim"             TO  PTBYO-DLT-OPID
               END-IF
           ELSE
      *
      *    転帰日
           MOVE    WRK-TENKIYMD        TO  PTBYO-TENKIYMD
      *    転帰区分
           MOVE    WRK-TENKI           TO  PTBYO-TENKIKBN
      *
      *    主病名フラグ
           MOVE    WRK-SYUBYOFLG       TO  PTBYO-SYUBYOFLG
      *
      *    疑いフラグ
           EVALUATE    PTBYO-UTAGAIFLG ALSO    WRK-UTAGAIFLG
               WHEN    SPACE           ALSO    "1"
               WHEN    "1"             ALSO    "1"
                   MOVE    "1"             TO  PTBYO-UTAGAIFLG
               WHEN    "2"             ALSO    "1"
               WHEN    "3"             ALSO    "1"
                   MOVE    "3"             TO  PTBYO-UTAGAIFLG
               WHEN    SPACE           ALSO    SPACE
               WHEN    "1"             ALSO    SPACE
                   MOVE    SPACE           TO  PTBYO-UTAGAIFLG
               WHEN    "2"             ALSO    SPACE
               WHEN    "3"             ALSO    SPACE
                   MOVE    "2"             TO  PTBYO-UTAGAIFLG
           END-EVALUATE
      *
      *    病名
           MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
      *
           INITIALIZE                      PTBYO-BYOMEICD-G
                                           PTBYO-KHNBUICD-G
      *
      *    疾患コードがエラー又は未コード化傷病名コード以外のとき
           IF      FLG-BYOMEI-CHK      NOT =   1   AND 5 AND 6
               MOVE    ZERO                TO  IDZ
                                               PTBYO-BYOMEICDSU
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       6
                       OR      WRK-TBYOMEICD (IDY) =   SPACE
      *            病名コード
                   MOVE    WRK-TBYOMEICD (IDY) 
                                           TO  PTBYO-BYOMEICD (IDY)
      *            疑いフラグ（病名コードより）
                   IF      PTBYO-UTAGAIFLG     =   "3"
                       CONTINUE
                   ELSE
                       EVALUATE    PTBYO-UTAGAIFLG ALSO
                                               WRK-TUTAGAIFLG (IDY)
                           WHEN    SPACE           ALSO    "1"
                           WHEN    SPACE           ALSO    "2"
                               MOVE    WRK-TUTAGAIFLG (IDY)
                                                   TO  PTBYO-UTAGAIFLG
                           WHEN    "2"             ALSO    "1"
                           WHEN    "1"             ALSO    "2"
                               MOVE    "3"         TO  PTBYO-UTAGAIFLG
                       END-EVALUATE
                   END-IF      
      *            病名コード数
                   ADD     1                   TO  PTBYO-BYOMEICDSU
      *            基本病名コード・基本部位コード
                   IF      PTBYO-BYOMEICD (IDY)(1:1)
                                               NOT =   "Z" 
                       MOVE    PTBYO-BYOMEICD (IDY)
                                           TO  PTBYO-KHNBYOMEICD
                   ELSE
                       IF      PTBYO-BYOMEICD (IDY) (1:4)
                                               =   "ZZZ1"
                           ADD     1       TO  IDZ
                           IF      IDZ         <   4
                               MOVE    PTBYO-BYOMEICD (IDY)
                                               TO  PTBYO-KHNBUICD(IDZ)
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
      *
      *        病名文字数
               PERFORM VARYING     IDY     FROM    80 BY  -1
                       UNTIL       IDY     <   1
                   IF      PTBYO-BYOMEI(IDY:1)     NOT =   SPACE
                       COMPUTE PTBYO-BYOMEIMOJI    =   IDY /   2
                       MOVE    1                   TO  IDY
                   END-IF
               END-PERFORM
      *
      *        病名編集フラグ（コードがエラーのとき、病名が長いとき）
               IF    ( FLG-BYOMEI-CHK  NOT =   ZERO  )
               OR    ( WRK-BYOMEI2     NOT =   SPACE )    
                   MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG
               END-IF
      *
      *        レセ電エラーフラグ（編集病名のとき）
               IF      PTBYO-BYOMEIHENFLG          =   "1"
                   IF      PTBYO-BYOMEIMOJI        >   20
                       MOVE    "1" TO  PTBYO-RECEDENFLG       
                   END-IF
               END-IF
      *
      *        レセ電エラーフラグ（疑いフラグが設定されているとき）
               IF    ( PTBYO-RECEDENFLG        =   SPACE ) 
               AND   ( PTBYO-UTAGAIFLG     NOT =   SPACE )
                   MOVE    SPACE               TO  WRK-HENFLG1
                                                   WRK-HENFLG2
      *     
      *            病名文字数を計算
                   COMPUTE WRK-MOJISU      =   PTBYO-BYOMEIMOJI       
                                           *   2
                   IF      PTBYO-UTAGAIFLG         =   "1" OR  "3"
                       IF      WRK-MOJISU      <   8
                           MOVE    1               TO  IDZ
                       ELSE    
                           COMPUTE IDZ     =   WRK-MOJISU    -   7
                       END-IF    
                       PERFORM VARYING IDY FROM    IDZ BY  2
                               UNTIL   IDY >       WRK-MOJISU
                           IF      PTBYO-BYOMEI        (IDY:2) =   "疑"
                               MOVE    "1"              TO  WRK-HENFLG1
                               MOVE    WRK-MOJISU       TO  IDY
                           END-IF
                       END-PERFORM
                   END-IF                
                   IF      PTBYO-UTAGAIFLG         =   "2" OR  "3"
                       PERFORM VARYING IDY FROM    1   BY  2
                               UNTIL   IDY >       WRK-MOJISU
                           IF      PTBYO-BYOMEI        (IDY:4) =  "急性"
                               MOVE    "1"              TO  WRK-HENFLG2
                               MOVE    WRK-MOJISU       TO  IDY
                           END-IF
                       END-PERFORM
                   END-IF
      *
      *            疑いフラグがセットされているとき
                   IF    ( PTBYO-UTAGAIFLG     =   "1" OR  "3" )
                   AND   ( WRK-HENFLG1         =   SPACE       )                         
                       COMPUTE WRK-MOJISU          =      
                                WRK-MOJISU +   WRK-CONS-UTAGAI-MOJISU
                       IF      PTBYO-BYOMEICD (21) NOT =   SPACE             
                           IF      WRK-MOJISU      >   20
                               MOVE    "1"         TO  PTBYO-RECEDENFLG       
                           END-IF
                       END-IF
                   END-IF    
                   IF    ( PTBYO-UTAGAIFLG     =   "2" OR  "3" )
                   AND   ( WRK-HENFLG2         =   SPACE       )                         
                       COMPUTE WRK-MOJISU          =      
                                WRK-MOJISU +   WRK-CONS-KYUSEI-MOJISU
                       IF      PTBYO-BYOMEICD (21) NOT =   SPACE             
                           IF      WRK-MOJISU      >   20
                               MOVE    "1"         TO  PTBYO-RECEDENFLG       
                           END-IF
                       END-IF
                   END-IF
               END-IF    
             ELSE
      *        疾患コードがエラー又は未コード化傷病名コードのとき
      *        病名からコード検索
               INITIALIZE                  ORCSBYOMEIAREA
               MOVE    "1"                 TO  LNK-BYO-SYORI
               MOVE    PTBYO-SRYYMD        TO  LNK-BYO-SRYYMD-IN
               MOVE    PTBYO-UTAGAIFLG     TO  LNK-BYO-UTAGAIFLG-IN
               MOVE    PTBYO-BYOMEI        TO  LNK-BYO-BYOMEI
               MOVE    PTBYO-SYUBYOFLG     TO  LNK-BYO-SYUBYOFLG 
               CALL    "ORCSBYOMEI"        USING    ORCSBYOMEIAREA
                                                    SPA-AREA
      *  
      *        疑いフラグ
               IF      PTBYO-UTAGAIFLG     =   "3"
                   CONTINUE
               ELSE
                   EVALUATE    PTBYO-UTAGAIFLG ALSO
                                           LNK-BYO-UTAGAIFLG
                       WHEN    SPACE           ALSO    "1"
                       WHEN    SPACE           ALSO    "2"
                           MOVE    LNK-BYO-UTAGAIFLG
                                                   TO  PTBYO-UTAGAIFLG
                       WHEN    "2"             ALSO    "1"
                       WHEN    "1"             ALSO    "2"
                           MOVE    "3"         TO  PTBYO-UTAGAIFLG
                   END-EVALUATE
               END-IF      
      *        病名コード
               PERFORM VARYING IDX4    FROM    1   BY  1
                       UNTIL   IDX4    >       LNK-BYO-BYOMEI-MAX
                   MOVE    LNK-BYO-TBYOMEICD (IDX4)
                                       TO  PTBYO-BYOMEICD  (IDX4)
               END-PERFORM
      *        病名コード数
               MOVE    LNK-BYO-BYOMEICDSU  TO  PTBYO-BYOMEICDSU
      *        基本病名コード
               MOVE    LNK-BYO-KHNBYOMEICD TO  PTBYO-KHNBYOMEICD
      *        基本部位コード
               MOVE    LNK-BYO-KHNBUICD (1)
                                           TO  PTBYO-KHNBUICD (1)
               MOVE    LNK-BYO-KHNBUICD (2)
                                           TO  PTBYO-KHNBUICD (2)
               MOVE    LNK-BYO-KHNBUICD (3)
                                           TO  PTBYO-KHNBUICD (3)
      *        レセ電エラーフラグ
               MOVE    LNK-BYO-RECEDENFLG  TO  PTBYO-RECEDENFLG
      *        病名文字数
               MOVE    LNK-BYO-BYOMEIMOJI  TO  PTBYO-BYOMEIMOJI
      *        病名編集フラグ
               IF    ( LNK-BYO-BYOMEICD-ERR    =   1  )
               OR    ( LNK-BYO-TANDOKUKBN-ERR  =   1  )
               OR    ( LNK-BYO-BYOMEIMOJI      >   80 )
      *            単独病名・修飾語コードのみ・病名が長いとき
                   MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG
               ELSE
                   MOVE    LNK-BYO-BYOMEIHENFLG
                                               TO  PTBYO-BYOMEIHENFLG
               END-IF
             END-IF
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
      *
           IF      LNK-KBN-CODE        =   "1"
               MOVE    "api"               TO  PTBYO-OPID
           ELSE
               MOVE    "claim"             TO  PTBYO-OPID
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC      NOT =   ZERO 
               DISPLAY "UPD PTBYOMEI-KEY =>" PTBYO-KEY  "."
               MOVE    "12"                TO  LNK-ERR2-CODE
               MOVE
                   "全ての病名の更新に失敗しました（更新時にエラー）"
                                           TO  LNK-ERR2-MSG 
           ELSE
               ADD     1                   TO  CNT-OUT
               IF      FLG-BYOMEI-DELETE   =   1
                   ADD     1                   TO  CNT-DELETE
               END-IF
           END-IF
           .
       2006124-KOUSIN-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ更新処理（同期フラグ設定時）
      *****************************************************************
       2006125-KOUSIN-03-SEC           SECTION.
      *
      *    患者病名マスタ検索（同一開始日・病名）
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
           MOVE    WRK-HOSOKU-COMMENT  TO  PTBYO-HOSOKU-COMMENT
           IF      FLG-BYOMEI-DELETE   =   1 
               MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
                                               PTBYO-XXBYOMEI
           ELSE
               IF      WRK-UTAGAIFLG-KETASU    =   ZERO
                   MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
                   STRING  WRK-BYOMEI1 (1:80)  DELIMITED BY  SPACE
                           "の疑い"            DELIMITED BY  SIZE  
                                               INTO    PTBYO-XXBYOMEI
                   END-STRING
               ELSE
                   IF      WRK-UTAGAIFLG-KETASU    =   1
                       MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
                                                       PTBYO-XXBYOMEI
                   ELSE
                       MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
                       STRING  WRK-BYOMEI1 (1:WRK-UTAGAIFLG-KETASU - 1)
                                                   DELIMITED BY  SIZE
                               WRK-BYOMEI1 (WRK-UTAGAIFLG-KETASU + 6:)
                                                   DELIMITED BY  SPACE
                                               INTO    PTBYO-XXBYOMEI
                       END-STRING
                   END-IF
               END-IF
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           IF    ( LNK-KBN-V2          =   "1" )
           AND   ( LNK-KBN-CODE        =   "1" )
               MOVE    "key47"             TO  WRK-PTBYO-MCP-PATHNAME
           ELSE
               MOVE    "key29"             TO  WRK-PTBYO-MCP-PATHNAME
           END-IF
           MOVE    WRK-PTBYO-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-PTBYO-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM         UNTIL   FLG-PTBYOMEI    =   1
               MOVE    PTBYO-SRYKA         TO  KEY-N-SRYKA
               INITIALIZE                  WRK-PTBYOMEI-DOUKI-REC
      *
               PERFORM         UNTIL   FLG-PTBYOMEI    =   1
                               OR      PTBYO-SRYKA NOT =   KEY-N-SRYKA
                   IF      WRK-PTBYO-DOUKI-RENNUM
                                               =   ZERO
                       IF    ( FLG-PTBYOMEI-OK     =   1               )
                       AND   ( WRK-NYUGAIKBN   NOT =   PTBYO-NYUGAIKBN )
                           CONTINUE
                       ELSE
                           MOVE    PTBYOMEI-REC    TO
                                               WRK-PTBYOMEI-DOUKI-REC
                       END-IF
                   END-IF 
                   IF      WRK-BYOMEI1 (1:80)  =   PTBYO-BYOMEI
                       IF    ( FLG-PTBYOMEI-OK     =   1               )
                       AND   ( WRK-NYUGAIKBN   NOT =   PTBYO-NYUGAIKBN )
                           CONTINUE
                       ELSE
                           MOVE    PTBYOMEI-REC    TO
                                               WRK-PTBYOMEI-DOUKI-REC
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
                   MOVE    WRK-PTBYO-MCP-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-PTBYO-READ-SEC
               END-PERFORM
      *
      *        対象病名の更新
               MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-TAIHI-REC
      *
      *        同一病名があるとき同じ診療科は更新しない
               IF    ( FLG-PTBYOMEI-OK     =   1                     )
               AND   ( WRK-SRYKA           =   WRK-PTBYO-DOUKI-SRYKA )
                   CONTINUE
               ELSE
                   MOVE    WRK-PTBYOMEI-DOUKI-REC
                                               TO  PTBYOMEI-REC
                   PERFORM 2006124-KOUSIN-02-SEC
               END-IF
      *
               MOVE    WRK-PTBYOMEI-TAIHI-REC
                                           TO  PTBYOMEI-REC 
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-PTBYO-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2006125-KOUSIN-03-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ更新処理（入外区分）
      *****************************************************************
       2006126-KOUSIN-04-SEC               SECTION.
      *
           IF      PTBYO-NYUGAIKBN     =   SPACE
               IF      WRK-NYUGAIKBN       =   SPACE
                   GO  TO  2006126-KOUSIN-04-EXT
               END-IF
           ELSE
               IF      WRK-NYUGAIKBN       =   SPACE
      *            同一病名が複数のときは更新しない
                   IF      PTBYO-CNT           >   1
                       GO  TO  2006126-KOUSIN-04-EXT
                   END-IF
               ELSE
                   IF      WRK-NYUGAIKBN   =   PTBYO-NYUGAIKBN
                       GO  TO  2006126-KOUSIN-04-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    入外区分
           MOVE    WRK-NYUGAIKBN       TO  PTBYO-NYUGAIKBN
      *
           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
      *
           IF      LNK-KBN-CODE        =   "1"
               MOVE    "api"               TO  PTBYO-OPID
           ELSE
               MOVE    "claim"             TO  PTBYO-OPID
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC      NOT =   ZERO 
               DISPLAY "UPD PTBYOMEI-KEY =>" PTBYO-KEY  "."
               MOVE    "13"                TO  LNK-ERR2-CODE
               MOVE
              "全ての病名の更新に失敗しました（入外区分更新時にエラー）"
                                           TO  LNK-ERR2-MSG 
           ELSE
               ADD     1                   TO  CNT-OUT
           END-IF
           .
       2006126-KOUSIN-04-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    患者病名マスタ更新処理（同期フラグ設定時の入外区分）
      *****************************************************************
       2006127-KOUSIN-05-SEC               SECTION.
      *
           INITIALIZE                  WRK-PTBYOMEI-REC
                                       WRK-PTBYOMEI-SRYKA-REC
                                       WRK-PTBYOMEI-DOUKI-REC
                                       PTBYOMEI-CNT
                                       PTBYOMEI-SRYKA-CNT
                                       PTBYOMEI-DOUKI-CNT
      *
      *    患者病名マスタ検索（同一開始日・病名）
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
           MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
                                           PTBYO-XXBYOMEI
           MOVE    WRK-HOSOKU-COMMENT  TO  PTBYO-HOSOKU-COMMENT
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           IF    ( LNK-KBN-V2          =   "1" )
           AND   ( LNK-KBN-CODE        =   "1" )
               MOVE    "key47"             TO  WRK-PTBYO-MCP-PATHNAME
           ELSE
               MOVE    "key29"             TO  WRK-PTBYO-MCP-PATHNAME
           END-IF
           MOVE    WRK-PTBYO-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-PTBYO-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM         UNTIL   FLG-PTBYOMEI    =   1
               MOVE    PTBYO-SRYKA         TO  KEY-N-SRYKA
               INITIALIZE                  WRK-PTBYOMEI-DOUKI-REC
                                           PTBYOMEI-DOUKI-CNT
      *
               PERFORM         UNTIL   FLG-PTBYOMEI    =   1
                               OR      PTBYO-SRYKA NOT =   KEY-N-SRYKA
      *
                   IF      WRK-PTBYO-DOUKI-RENNUM
                                               =   ZERO
                       MOVE    PTBYOMEI-REC    TO WRK-PTBYOMEI-DOUKI-REC
                   END-IF 
                   ADD     1                   TO  PTBYOMEI-DOUKI-CNT
      *
                   MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
                   MOVE    WRK-PTBYO-MCP-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-PTBYO-READ-SEC
               END-PERFORM
      *
      *        対象病名の更新
               MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-TAIHI-REC
      *
               MOVE    WRK-PTBYOMEI-DOUKI-REC
                                           TO  PTBYOMEI-REC
               MOVE    PTBYOMEI-DOUKI-CNT  TO  PTBYO-CNT
               PERFORM 2006126-KOUSIN-04-SEC
      *
               MOVE    WRK-PTBYOMEI-TAIHI-REC
                                           TO  PTBYOMEI-REC 
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-PTBYO-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
           .
       2006127-KOUSIN-05-EXT.
           EXIT.
      *
      *****************************************************************
      *    補足コメントチェック処理
      *****************************************************************
       2006128-HOSOKU-COMMENT-CHK-SEC  SECTION.
      *
           DISPLAY " disease_sname ="  LF04-HOSOKU-COMMENT (IDX)
      *
           MOVE    SPACE               TO  WRK-HOSOKU-COMMENT
           MOVE    ZERO                TO  FLG-HOSOKU-CHK
      *
           IF      LF04-HOSOKUCD-G (IDX) =   SPACE
               MOVE    LF04-HOSOKU-COMMENT (IDX)
                                             TO  WRK-HOSOKU-COMMENT
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       3
                       OR      LF04-HOSOKUCD (IDX IDY) 
                                   =       SPACE
      *
                   DISPLAY " IDY="   IDY
                   DISPLAY " disease_scode ="
                                     LF04-HOSOKUCD (IDX IDY)
      *
                   MOVE    LF04-HOSOKUCD (IDX IDY)
                                           TO  WRK-THOSOKUCD (IDY)
               END-PERFORM
      *
      *        補足コメントコードチェック
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       3
                       OR      WRK-THOSOKUCD (IDY) =   SPACE
                       OR      FLG-HOSOKU-CHK      =   2
                   MOVE    ZERO                TO  FLG-HOSOKU-CHK
      *
      *            病名コードの長さチェック
                   MOVE    ZERO                TO  WRK-LENGTH 
                   PERFORM VARYING IDZ FROM    10  BY  -1
                           UNTIL   IDZ <       1
                       IF      WRK-THOSOKUCD (IDY)(IDZ:1)
                                               NOT =   SPACE
                           MOVE    IDZ             TO  WRK-LENGTH
                           MOVE    1               TO  IDZ
                       END-IF
                   END-PERFORM
                   EVALUATE    WRK-LENGTH
                       WHEN    4
                           MOVE    SPACE       TO  WRK-BYOMEICD
                           MOVE    "ZZZ"       TO  WRK-BYOMEICD (1:3)
                           MOVE    WRK-THOSOKUCD (IDY)(1:4)
                                               TO  WRK-BYOMEICD (4:4)
                           MOVE    WRK-BYOMEICD
                                               TO  WRK-THOSOKUCD (IDY)
                       WHEN    7
                           CONTINUE
                       WHEN    OTHER            
                           MOVE    1           TO  FLG-HOSOKU-CHK
                   END-EVALUATE
      *
      *            病名マスタチェック
                   IF      FLG-HOSOKU-CHK      =   ZERO
                       INITIALIZE                      BYOMEI-REC
                       MOVE    WRK-THOSOKUCD (IDY) TO  BYO-BYOMEICD
                       PERFORM 900-BYOMEI-READ-SEC
                       IF      FLG-BYOMEI          =   ZERO  
                           MOVE    BYO-BYOMEI      TO
                                           WRK-THOSOKU-COMMENT (IDY)
                       ELSE
                           MOVE    1               TO  FLG-HOSOKU-CHK
                       END-IF
                   END-IF
                   IF      FLG-HOSOKU-CHK      =   1
      *                自院病名チェック
                       INITIALIZE              USERBYOMEI-REC
                       MOVE    WRK-THOSOKUCD (IDY)
                                               TO  USBYO-BYOMEIINPUTCD
                       PERFORM 900-USBYO-INV-SEC
                       IF      FLG-USERBYOMEI  =   ZERO
                           MOVE    USBYO-BYOMEI    TO
                                               WRK-THOSOKU-COMMENT (IDY)
                           MOVE    ZERO            TO  FLG-HOSOKU-CHK
                       ELSE
                           MOVE    2               TO  FLG-HOSOKU-CHK
                           INITIALIZE                  WRK-HOSOKU-TBL
                       END-IF
                   END-IF
               END-PERFORM
      *
      *        補足コメントを組み立てる
               MOVE    SPACE                   TO  WRK-HOSOKU-COMMENT
               IF      FLG-HOSOKU-CHK      =   ZERO
                   STRING  WRK-THOSOKU-COMMENT (1) DELIMITED   BY  SPACE
                           WRK-THOSOKU-COMMENT (2) DELIMITED   BY  SPACE
                           WRK-THOSOKU-COMMENT (3) DELIMITED   BY  SPACE
                                           INTO    WRK-HOSOKU-COMMENT
                   END-STRING
               ELSE
                   MOVE    LF04-HOSOKU-COMMENT (IDX)
                                               TO  WRK-HOSOKU-COMMENT
               END-IF
           END-IF
      *
      *    全角チェック
           IF      WRK-HOSOKU-COMMENT  NOT =   SPACE
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI 
               MOVE    WRK-HOSOKU-COMMENT  TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "2"  )
                   MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                               TO  WRK-HOSOKU-COMMENT
               ELSE
                   MOVE    "W05"                TO  WRK-DIAG-WARNING
               END-IF
      *
               IF  WRK-DIAG-WARNING        =   SPACE
      *            改行コードチェック
                   MOVE    ZERO                TO  WRK-COL
                   INSPECT WRK-HOSOKU-COMMENT  TALLYING    WRK-COL
                                               FOR ALL WRK-CONS-RETURN
                   IF      WRK-COL             >   ZERO
                       MOVE    "W06"               TO  WRK-DIAG-WARNING
                   END-IF
               END-IF
      *
               IF  WRK-DIAG-WARNING    NOT =   SPACE
                   MOVE    "？？？？？"        TO  WRK-HOSOKU-COMMENT
                   PERFORM 500-WARNING-HENSYU-SEC
               END-IF
           END-IF
      *
      *    前後の（）をはずす
           IF      WRK-HOSOKU-COMMENT  NOT =   SPACE    
               MOVE    SPACE               TO  WRK-HOSOKU-COMMENT1
               COMPUTE IDY     =   KANACHK-MAX     -   1
               IF      WRK-HOSOKU-COMMENT (IDY:2)
                                               =   "）"
                   COMPUTE IDZ     =   KANACHK-MAX     -   2
                   MOVE    WRK-HOSOKU-COMMENT (1:IDZ)
                                               TO  WRK-HOSOKU-COMMENT1
                   MOVE    WRK-HOSOKU-COMMENT1 TO  WRK-HOSOKU-COMMENT
               END-IF
               MOVE    SPACE               TO  WRK-HOSOKU-COMMENT1
               IF      WRK-HOSOKU-COMMENT (1:2)
                                               =   "（"
                   MOVE    WRK-HOSOKU-COMMENT (3:)
                                               TO  WRK-HOSOKU-COMMENT1
                   MOVE    WRK-HOSOKU-COMMENT1 TO  WRK-HOSOKU-COMMENT
               END-IF
           END-IF 
      *
           DISPLAY " diagnosis supplement ="  WRK-HOSOKU-COMMENT
      *
           .
       2006128-HOSOKU-COMMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一病名チェック処理
      *****************************************************************
       2008-BYOMEI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-PTBYOMEI-OK
           INITIALIZE                      OK-PTBYOMEI-REC
      *
           IF      FLG-BYOMEI-DELETE   =   1
               GO  TO  2008-BYOMEI-CHK-EXT
           END-IF
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
           MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
                                           PTBYO-XXBYOMEI
           MOVE    WRK-HOSOKU-COMMENT  TO  PTBYO-HOSOKU-COMMENT
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key47"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key47"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM                 UNTIL   FLG-PTBYOMEI    =   1
                                   OR      FLG-PTBYOMEI-OK =   1
               IF    ( WRK-NYUGAIKBN       =   PTBYO-NYUGAIKBN )
               AND   ( WRK-SRYKA           =   PTBYO-SRYKA     )
                   MOVE    1                   TO  FLG-PTBYOMEI-OK
                   MOVE    PTBYOMEI-REC        TO  OK-PTBYOMEI-REC
               END-IF
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key47"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key47"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2008-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マスタとの重複チェック処理（転帰等）
      *****************************************************************
       2009-BYOMEI-DABURI-CHK-SEC  SECTION.
      *
           MOVE    SPACE           TO  WRK-ERR-SRYYMD
                                       WRK-ERR-SRYKA
                                       WRK-CHK-SRYKA
           INITIALIZE                  CHK-PTBYOMEI-REC
      *
           IF      FLG-BYOMEI-DELETE   =   1
               GO  TO  2009-BYOMEI-DABURI-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-DABURI-CHK
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-SRYKA           TO  PTBYO-SRYKA
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
           MOVE    WRK-HOSOKU-COMMENT  TO  PTBYO-HOSOKU-COMMENT
           MOVE    WRK-NYUGAIKBN       TO  PTBYO-NYUGAIKBN
           IF      WRK-NYUGAIKBN       =   SPACE
               MOVE    "key40"             TO  WRK-PTBYO-MCP-PATHNAME
           ELSE
               MOVE    "key41"             TO  WRK-PTBYO-MCP-PATHNAME
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-PTBYO-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-PTBYO-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM               UNTIL   FLG-PTBYOMEI    =   1
                                 OR      FLG-DABURI-CHK  >   0
               IF      CHK-PTBYO-SRYYMD    =   PTBYO-SRYYMD
               AND     CHK-PTBYO-SRYKA     =   PTBYO-SRYKA
               AND     CHK-PTBYO-RENNUM    =   PTBYO-RENNUM
                   CONTINUE
               ELSE
                   EVALUATE    TRUE
      *****           WHEN    WRK-BYOYMD  =   PTBYO-SRYYMD
      *****                MOVE    2               TO  FLG-DABURI-CHK
                       WHEN    WRK-BYOYMD  <   PTBYO-SRYYMD
                           IF      WRK-TENKIYMD    =   SPACE
                               MOVE    1       TO  FLG-DABURI-CHK
                           ELSE
                               IF     WRK-TENKIYMD
                                                   >   PTBYO-SRYYMD
                                   MOVE    1       TO  FLG-DABURI-CHK
                               END-IF
                           END-IF
                       WHEN    WRK-BYOYMD  >   PTBYO-SRYYMD
                           IF      PTBYO-TENKIYMD  =   SPACE
                               MOVE    1       TO  FLG-DABURI-CHK
                           ELSE
                               IF      WRK-BYOYMD  <   PTBYO-TENKIYMD
                                   MOVE    1       TO  FLG-DABURI-CHK
                               END-IF
                           END-IF
                   END-EVALUATE
      *
                   IF      FLG-DABURI-CHK      >   0
      *                同一病名における保険限定の重複チェック
                       PERFORM 20091-HKNCOMBI-DABURI-CHK-SEC
                       IF      FLG-HKNCOMBI-DABURI =   1
                           MOVE    ZERO        TO  FLG-DABURI-CHK
                       END-IF
                       IF      FLG-DABURI-CHK      >   0
                           MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    LNK-DAY2-EDTYMD3
                                                   TO  WRK-ERR-SRYYMD
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-PTBYO-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-PTBYO-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-DABURI-CHK      >   0
               MOVE    9                   TO  FLG-BYOMEI-CHK
      *
               IF      FLG-DABURI-CHK      =   1
                   MOVE    "E01"               TO  WRK-DIAG-WARNING
      *********ELSE
      *********    MOVE    "E02"               TO  WRK-DIAG-WARNING
               END-IF 
               PERFORM 500-WARNING-HENSYU-SEC
               GO  TO 2009-BYOMEI-DABURI-CHK-EXT
           END-IF
      *
      *    同期・集約フラグの設定がないとき
           IF    ( SYS-9000-BYOMEI-SYU-FLG
                                        =   SPACE )
           AND   ( SYS-9000-BYOMEI-DOUKI-FLG
                                        =   SPACE )
               GO  TO 2009-BYOMEI-DABURI-CHK-EXT
           END-IF
      *
      *    他診療科の同一病名の警告チェック
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-SRYKA           TO  PTBYO-SRYKA
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
           MOVE    WRK-HOSOKU-COMMENT  TO  PTBYO-HOSOKU-COMMENT
           MOVE    WRK-NYUGAIKBN       TO  PTBYO-NYUGAIKBN
           IF      WRK-NYUGAIKBN       =   SPACE
               MOVE    "key42"             TO  WRK-PTBYO-MCP-PATHNAME
           ELSE
               MOVE    "key43"             TO  WRK-PTBYO-MCP-PATHNAME
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-PTBYO-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-PTBYO-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM               UNTIL   FLG-PTBYOMEI    =   1
                                 OR      FLG-DABURI-CHK  >   0
               IF      CHK-PTBYO-SRYYMD    =   PTBYO-SRYYMD
               AND     CHK-PTBYO-SRYKA     =   PTBYO-SRYKA
               AND     CHK-PTBYO-RENNUM    =   PTBYO-RENNUM
                   CONTINUE
               ELSE
                   EVALUATE    TRUE
                       WHEN    WRK-BYOYMD  =   PTBYO-SRYYMD
      *****************    MOVE    1               TO  FLG-DABURI-CHK
                           MOVE    PTBYO-SRYKA     TO  WRK-CHK-SRYKA
                       WHEN    WRK-BYOYMD  <   PTBYO-SRYYMD
                           IF      WRK-TENKIYMD    =   SPACE
                               MOVE    1       TO  FLG-DABURI-CHK
                           ELSE
                               IF     WRK-TENKIYMD
                                                   >   PTBYO-SRYYMD
                                   MOVE    1       TO  FLG-DABURI-CHK
                               END-IF
                           END-IF
                       WHEN    WRK-BYOYMD  >   PTBYO-SRYYMD
                           IF      PTBYO-TENKIYMD  =   SPACE
                               MOVE    1       TO  FLG-DABURI-CHK
                           ELSE
                               IF      WRK-BYOYMD  <   PTBYO-TENKIYMD
                                   MOVE    1       TO  FLG-DABURI-CHK
                               END-IF
                           END-IF
                   END-EVALUATE
      *
                   IF      FLG-DABURI-CHK      >   0
      *                同一病名における保険限定の重複チェック
                       PERFORM 20091-HKNCOMBI-DABURI-CHK-SEC
                       IF      FLG-HKNCOMBI-DABURI =   1
                           MOVE    ZERO        TO  FLG-DABURI-CHK
                       END-IF
                       IF      FLG-DABURI-CHK      >   0
                           IF    ( SYS-9000-BYOMEI-SYU-FLG
                                                       =   "1"   )
                           AND   ( SYS-9000-BYOMEI-DOUKI-FLG
                                                       =   SPACE )
                           AND   ( PTBYO-SRYKA     NOT =
                                                   WRK-CHK-SRYKA )
      *                        集約フラグのみ設定時に最小診療科でないとき
      *                        はエラーとしない
                               MOVE    ZERO            TO
                                                       FLG-DABURI-CHK
                           ELSE
                               MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
                               PERFORM 5002-HIZUKE-HEN-SEC
                               MOVE    LNK-DAY2-EDTYMD3
                                                       TO
                                                       WRK-ERR-SRYYMD
                               PERFORM 5001-SRYKA-HENSYU-SEC
                               MOVE    SYS-1005-SRYKANAME1
                                                       TO  WRK-ERR-SRYKA
                           END-IF 
                       END-IF
                   END-IF
               END-IF
      *
      *        最小診療科の設定
               IF      WRK-BYOYMD          =   PTBYO-SRYYMD
                   MOVE    PTBYO-SRYKA         TO  WRK-CHK-SRYKA
               END-IF
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-PTBYO-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-PTBYO-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-DABURI-CHK      >   0
               IF      SYS-9000-BYOMEI-DOUKI-FLG
                                           =   "1"
                   MOVE    9                   TO  FLG-BYOMEI-CHK
                   MOVE    "E07"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
                   GO  TO 2009-BYOMEI-DABURI-CHK-EXT
               END-IF
               IF      SYS-9000-BYOMEI-SYU-FLG
                                           =   "1"
                   MOVE    9                   TO  FLG-BYOMEI-CHK
                   MOVE    "E07"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
                   GO  TO 2009-BYOMEI-DABURI-CHK-EXT
               END-IF
           END-IF
           .
       2009-BYOMEI-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一病名における保険限定の重複チェック処理
      *****************************************************************
       20091-HKNCOMBI-DABURI-CHK-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI-DABURI
      *
      *    保険限定の設定がない場合はチェックしない
           IF    ( PTBYO-HKNCOMBI      =   ZERO )
           AND   ( PTBYO-KOHID         =   ZERO )
               GO  TO  20091-HKNCOMBI-DABURI-CHK-EXT
           END-IF
      *
      *    保険組合せの重複チェック
           IF      PTBYO-HKNCOMBI      =   ZERO
             CONTINUE
           ELSE
               MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    WRK-PTID            TO  COMB-PTID
               MOVE    PTBYO-HKNCOMBI      TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM 900-COMB-READ-SEC
      *        労災または自賠責または公害の場合は登録可能とする
               EVALUATE    COMB-HKNNUM
                   WHEN    "971"
                   WHEN    "973"
                   WHEN    "975"
                       MOVE    1               TO  FLG-HKNCOMBI-DABURI
                   WHEN    OTHER
                       MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
               END-EVALUATE
      *
      *        保険組合せで登録不可のとき
               IF      FLG-HKNCOMBI-DABURI =   ZERO
                   GO  TO   20091-HKNCOMBI-DABURI-CHK-EXT
               END-IF
           END-IF
      *
      *    第三者行為の重複チェック
           IF      PTBYO-KOHID     =   ZERO
               CONTINUE
           ELSE 
      *        第三者行為設定時は登録不可とする
               MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
           END-IF
           .                   
       20091-HKNCOMBI-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    警告編集処理
      *****************************************************************
       500-WARNING-HENSYU-SEC                 SECTION.
      *
           ADD     1                   TO  CNT-DIAG
      *
           MOVE    IDX                 TO  LNK-DIAG-POSITION (CNT-DIAG)
           MOVE    WRK-DIAG-WARNING    TO  LNK-DIAG-WARNING  (CNT-DIAG)
           MOVE    WRK-BYOMEI1 (1:80)  TO  LNK-DIAG-NAME     (CNT-DIAG)
           EVALUATE    WRK-DIAG-WARNING
               WHEN    "E01"
                   STRING  "同名の病名が"      DELIMITED   BY  SIZE 
                           WRK-ERR-SRYYMD      DELIMITED   BY  SPACE
                           "に存在します（転帰日等を確認して下さい）"
                                               DELIMITED   BY  SIZE 
                                       INTO    LNK-DIAG-MSG (CNT-DIAG)
                   END-STRING
               WHEN    "E02"
                   STRING  "同名の病名が"      DELIMITED   BY  SIZE 
                           WRK-ERR-SRYYMD      DELIMITED   BY  SPACE
                           "に存在します"      DELIMITED   BY  SIZE 
                                       INTO    LNK-DIAG-MSG (CNT-DIAG)
                   END-STRING
               WHEN    "E03"
                   MOVE    "病名コードが不正です"
                                       TO  LNK-DIAG-MSG    (CNT-DIAG)
               WHEN    "E04"
                   MOVE    "補足コメントコードが不正です"
                                       TO  LNK-DIAG-MSG    (CNT-DIAG)
               WHEN    "E05"
                   STRING  "同名の病名が"      DELIMITED   BY  SIZE 
                           WRK-ERR-SRYKA       DELIMITED   BY  SPACE
                           "に"                DELIMITED   BY  SIZE 
                           "複数存在します"    DELIMITED   BY  SIZE 
                                       INTO    LNK-DIAG-MSG (CNT-DIAG)
                   END-STRING
               WHEN    "E06"
                   MOVE    "削除対象の病名がありません"
                                       TO  LNK-DIAG-MSG    (CNT-DIAG)
               WHEN    "E07"
                   STRING  "同名の病名が"      DELIMITED   BY  SIZE 
                           WRK-ERR-SRYKA       DELIMITED   BY  SPACE
                           "に"                DELIMITED   BY  SIZE 
                           WRK-ERR-SRYYMD      DELIMITED   BY  SPACE
                           "に存在します"      DELIMITED   BY  SIZE 
                                       INTO    LNK-DIAG-MSG (CNT-DIAG)
                   END-STRING
               WHEN    "W01"
                   MOVE    "廃止・移行先・推奨のある病名が存在します"
                                       TO  LNK-DIAG-MSG    (CNT-DIAG)
                   MOVE    WRK-DIAG-CHANGE
                                       TO  LNK-DIAG-CHANGE (CNT-DIAG)
               WHEN    "W02"
                   MOVE    "単独使用禁止病名です"
                                       TO  LNK-DIAG-MSG    (CNT-DIAG)
               WHEN    "W03"
                   MOVE
                   "全角チェックでエラーとなる文字が病名に存在します"
                                       TO  LNK-DIAG-MSG    (CNT-DIAG)
               WHEN    "W04"
                   MOVE    "病名に改行コードが存在します"
                                       TO  LNK-DIAG-MSG    (CNT-DIAG)
               WHEN    "W05"
                   MOVE
              "全角チェックでエラーとなる文字が補足コメントに存在します"
                                       TO  LNK-DIAG-MSG    (CNT-DIAG)
               WHEN    "W06"
                   MOVE    "補足コメントに改行コードが存在します"
                                       TO  LNK-DIAG-MSG    (CNT-DIAG)
           END-EVALUATE
           EVALUATE    WRK-DIAG-WARNING
               WHEN    "W01"
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       7
                           OR      PTBYO-BYOMEICD (IDXX)   =   SPACE
                       MOVE    LNK-DIAG-CODE (CNT-DIAG)
                                                   TO  WRK-DIAG-CODE
                       MOVE    SPACE               TO
                                               LNK-DIAG-CODE (CNT-DIAG) 
                       MOVE    PTBYO-BYOMEICD (IDXX)
                                                   TO  WRK-BYOMEICD
                       IF       PTBYO-BYOMEICD (IDXX) (1:3)
                                               =   "ZZZ"
                           MOVE    PTBYO-BYOMEICD (IDXX) (4:)
                                                   TO  WRK-BYOMEICD
                       END-IF
                       IF      IDXX                =   1
                           MOVE    WRK-BYOMEICD        TO
                                               LNK-DIAG-CODE (CNT-DIAG)
                       ELSE
                           STRING  WRK-DIAG-CODE   DELIMITED  BY  SPACE
                                   "."
                                   WRK-BYOMEICD    DELIMITED  BY  SPACE
                                       INTO    LNK-DIAG-CODE (CNT-DIAG)
                           END-STRING
                       END-IF
                   END-PERFORM
               WHEN    OTHER
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       7
                           OR      WRK-TSPICOD (IDXX)   =   SPACE
                       MOVE    LNK-DIAG-CODE (CNT-DIAG)
                                                   TO  WRK-DIAG-CODE
                       MOVE    SPACE               TO
                                               LNK-DIAG-CODE (CNT-DIAG) 
                       IF      IDXX                =   1
                           MOVE    WRK-TSPICOD (IDXX)  TO
                                               LNK-DIAG-CODE (CNT-DIAG)
                       ELSE
                           STRING  WRK-DIAG-CODE   DELIMITED  BY  SPACE
                                   "."
                                   WRK-TSPICOD (IDXX)
                                                   DELIMITED  BY  SPACE
                                       INTO    LNK-DIAG-CODE (CNT-DIAG)
                           END-STRING
                       END-IF
                   END-PERFORM
           END-EVALUATE
      *
           IF      FLG-DIAG            =   9
               CONTINUE
           ELSE
               EVALUATE    WRK-DIAG-WARNING (1:1)
                   WHEN    "W"
                       MOVE    1                   TO  FLG-DIAG
                   WHEN    "E"
                       MOVE    9                   TO  FLG-DIAG
               END-EVALUATE
           END-IF
           MOVE    SPACE               TO  WRK-DIAG-AREA
           .
       500-WARNING-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    同義語マスタチェック処理
      *****************************************************************
       5001-SYNONYM-CHK-SEC            SECTION.
      *
           PERFORM 900-SYNONYM-READ-SEC
      *
           IF      FLG-SYNONYM         =   ZERO
               MOVE    SYNONYM-BYOMEICD    TO  BYO-BYOMEICD
               MOVE    ZERO                TO  FLG-BYOMEI
               PERFORM 900-BYOMEI-READ-SEC
               IF      FLG-BYOMEI          =   1
                   MOVE    1                   TO  FLG-SYNONYM
               ELSE
                   IF      BYO-HAISIYMD        =   SPACE   OR  ALL "9"
                       CONTINUE
                   ELSE    
                       MOVE    1                   TO  FLG-SYNONYM
                   END-IF
               END-IF
           END-IF
      *
           .
       5001-SYNONYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
               MOVE    SPACE               TO  WRK-NENGO
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       5001-SRYKA-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    PTBYO-SRYKA         TO  SYS-1005-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1005-REC
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
           ELSE
               INITIALIZE                  SYS-1005-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       5001-SRYKA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM  920-DBFETCH-SEC 
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  BYOMEI-REC
                   MOVE    ZERO                TO  FLG-BYOMEI
               ELSE
                   MOVE    1                   TO  FLG-BYOMEI
                   INITIALIZE                  BYOMEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           PERFORM  920-DBFETCH-SEC 
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読込（主キー）
      *****************************************************************
       900-PTBYO-INV-SEC         SECTION.
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
               INITIALIZE                  PTBYOMEI-REC
           END-IF
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTBYO-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読込
      *****************************************************************
       900-PTBYO-READ-SEC           SECTION.
      *
           PERFORM  920-DBFETCH-SEC 
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    ZERO            TO  FLG-PTBYOMEI
               IF      FLG-BYOMEI-DELETE   =   1
                   IF    ( WRK-TENKIYMD    =   PTBYO-TENKIYMD  )
                   AND   ( WRK-UTAGAIFLG   =   PTBYO-UTAGAIFLG )
                   AND   ( WRK-NYUGAIKBN   =   PTBYO-NYUGAIKBN )
                       CONTINUE
                   ELSE
                       GO  TO  900-PTBYO-READ-SEC
                   END-IF
               END-IF
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読込
      *****************************************************************
       900-PTBYO-READ-N-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-PTBYO-READ-N-EXT.
           EXIT.
      *****************************************************************
      *    自院病名マスター読込（主キー）
      *****************************************************************
       900-USBYO-INV-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
                   MOVE    ZERO            TO  FLG-USERBYOMEI
               ELSE
                   INITIALIZE                  USERBYOMEI-REC
                   MOVE    1               TO  FLG-USERBYOMEI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-USERBYOMEI
               INITIALIZE                  USERBYOMEI-REC
           END-IF
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-USBYO-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    同義語マスタ読込
      *****************************************************************
       900-SYNONYM-READ-SEC         SECTION.
      *
           MOVE    SYNONYM-REC         TO  MCPDATA-REC
           MOVE    "tbl_synonym"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_synonym"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYNONYM-REC
                   MOVE    ZERO            TO  FLG-SYNONYM
               ELSE
                   INITIALIZE                  SYNONYM-REC
                   MOVE    1               TO  FLG-SYNONYM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYNONYM
               INITIALIZE                      SYNONYM-REC
           END-IF
      *
           MOVE    "tbl_synonym"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYNONYM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       900-COMB-READ-SEC                SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-COMB-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
