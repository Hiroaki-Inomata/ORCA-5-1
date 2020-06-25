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
       PROGRAM-ID.                 ORCSSKJKEEP.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 食事負担据え置き対象取得（平成２８年改定）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/02/16    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      *平成２９年１０月改正対応
      *  05.00.00    NACL-小豆沢  17/08/16  ２９年１０月以降は患者負担
      *                                     コードが細分化されることから
      *                                     低所得者で難病据え置き時は
      *                                     返却する区分をクリアしない
      *              NACL-小豆沢  17/10/19  難病判定用の保険組合せ番号
      *                                     退避時に保険組合せ有効期間
      *                                     を考慮するよう修正
      *              NACL-小豆沢  17/11/29  低所得者の食事負担据え置き
      *                                     区分のクリアを平成２９年９月
      *                                     までとするよう対処
      *              NACL-小豆沢  18/06/06  精神病床長期入院のシステム
      *                                     予約コード入力回数チェック追加
      *              NACL-小豆沢  18/11/14  難病判定に急性増悪のシステム
      *                                     予約コード追加
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
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-SRYACT                  PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-TANKI2                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-PTKOHINF                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-SEISIN-CHK              PIC 9(01).
           03  FLG-SEISIN-SANTEI           PIC 9(01).
           03  FLG-NANBYO                  PIC 9(01).
           03  FLG-PTCOM                   PIC 9(01).
           03  FLG-SEISIN-IGAI             PIC 9(01).
           03  FLG-KEIZOKU                 PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX-ST                      PIC 9(02).
           03  IDX-ED                      PIC 9(02).
           03  IDX-TK3                     PIC 9(02).
           03  IDX-SYS                     PIC 9(03).
           03  IDX-NANBYO                  PIC 9(03).
           03  IDX-HKNCOMBI                PIC 9(03).
           03  IDX-YYK                     PIC 9(03).
           03  IDX-ACCT                    PIC 9(02).
      *
      *    コンスタント値
       01  CONSTANT-AREA.
      *    難病公費番号
           03   CONS-NANBYO                PIC X(03) VALUE "054".
      *    小児慢性特定疾患公費番号
           03   CONS-SHOUNI                PIC X(03) VALUE "052".
      *    システム予約−精神病床食事負担据え置き対象
           03   CONS-SEISIN-ARI            PIC X(09) VALUE "099999926".
      *    システム予約−難病食事負担据え置き
           03   CONS-NANBYO-ARI           PIC X(09) VALUE "099999927".
      *    システム予約−難病食事負担据え置き対象外
           03   CONS-NANBYO-NASI          PIC X(09) VALUE "099999928".
      *    システム予約−難病食事負担据え置き（急性増悪による転棟）
           03   CONS-NANBYO-ARI2          PIC X(09) VALUE "099999934".
      *    標準負担額経過措置（精神）入院年月日コメント：平成
           03   CONS-SEISIN-COMMENT-H      PIC X(09) VALUE "840000136".
      *    標準負担額経過措置（精神）入院年月日コメント：昭和
           03   CONS-SEISIN-COMMENT-S      PIC X(09) VALUE "840000140".
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-HKNCOMBI-DAY            PIC 9(03)   OCCURS  31.
           03  WRK-GAIHAKU-SRYCD           PIC X(09).
      *
      *    精神病棟の番号退避領域
           03  WRK-CHKBTU-TAIHI-TBL        OCCURS   300.
               05  WRK-CHKBTU-NUM          PIC X(03).
               05  WRK-CHKBTU-STYUKYMD     PIC X(08).
               05  WRK-CHKBTU-EDYUKYMD     PIC X(08).
      *
      *    ２７年４月１日から２８年４月１日に精神病棟入院期間の入院履歴番号
           03  WRK-RRKNUM                  PIC 9(03).
           03  WRK-RRKNUM2                 PIC 9(03).
           03  WRK-NYUINYMD                PIC X(08).
           03  WRK-NYUINYMDR   REDEFINES   WRK-NYUINYMD.
               05  WRK-NYUIN-YYMM          PIC 9(06).
               05  WRK-NYUIN-DD            PIC 9(02).
           03  WRK-TAIINYMD                PIC X(08).
           03  WRK-TAIINYMDR   REDEFINES   WRK-TAIINYMD.
               05  WRK-TAIIN-YYMM          PIC 9(06).
               05  WRK-TAIIN-DD            PIC 9(02).
      *    精神病棟入院開始日
           03  WRK-SEI-NYUINYMD            PIC X(08).
      *
           03  WRK-MATCH                   PIC X(02).
           03  WRK-MATCH2                  PIC X(02).
           03  WRK-MATCH3                  PIC X(02).
           03  WRK-SHONUM                  PIC 9(03).
      *
      *    システム予約コードの入力日と該当する入院履歴番号
           03  WRK-SYSYYK-TBL              OCCURS  21.
               05  WRK-SYSYYK-SRYYMD.
                  07  WRK-SYSYYK-SRYYM     PIC X(06).
                  07  WRK-SYSYYK-SRYDD     PIC X(02).
               05  WRK-SYSYYK-RRKNUM       PIC 9(03).
               05  WRK-SYSYYK-NYUINYMD.
                  07  WRK-SYSYYK-NYUINYM   PIC X(06).
                  07  WRK-SYSYYK-NYUINDD   PIC X(02).
               05  WRK-SYSYYK-TAIINYMD.
                  07  WRK-SYSYYK-TAIINYM   PIC X(06).
                  07  WRK-SYSYYK-TAIINDD   PIC X(02).
      *
      *    難病、小児慢性特定疾病の公費ＩＤ退避領域
           03  WRK-NANBYO-KOHID-TAIHI-TBL.
               05  WRK-NANBYO-KOHID        PIC 9(10)  OCCURS   10.
      *    難病、小児慢性特定疾病の保険組合せ番号退避領域
           03  WRK-NANBYO-HKNCOMBI-TAIHI-TBL.
               05  WRK-NANBYO-HKNCOMBI     PIC 9(04)  OCCURS   10.
      *
      *    難病、小児慢性特定疾病の食事据え置き指定（システム予約）
           03  WRK-NANBYO-INPUT-TAIHI-TBL.
               05  WRK-NANBYO-INPUT-OK    PIC X(01)  OCCURS   31.
               05  WRK-NANBYO-INPUT-NG    PIC X(01)  OCCURS   31.
      *
      *    同日再入院日の翌日取得用
           03  WRK-DOUJITSU-YMD            PIC X(08).
           03  WRK-DOUJITSU-YMDR    REDEFINES   WRK-DOUJITSU-YMD.
               05  WRK-DOUJITSU-YYMM       PIC 9(06).
               05  WRK-DOUJITSU-DD         PIC 9(02).
      *
      *    同日再入院日の翌日取得用
           03  WRK-YOKUJITSU-YMD          PIC 9(08).
           03  WRK-YOKUJITSU-YMDR   REDEFINES   WRK-YOKUJITSU-YMD.
               05  WRK-YOKUJITSU-YYMM      PIC 9(06).
               05  WRK-YOKUJITSU-DD        PIC 9(02).
      *
      *    月末日取得用
           03  WRK-GETSUMATSU-YMD.
               05  WRK-GETSUMATSU-YY       PIC 9(04).
               05  WRK-GETSUMATSU-MM       PIC 9(02).
               05  WRK-GETSUMATSU-DD       PIC 9(02).
      *
      *    短期滞在以外で入院料を算定している日
           03  WRK-TANKI-IGAI              PIC X(01)  OCCURS 31.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
           COPY    "CPSK5000.INC".
           COPY    "CPSK5001.INC".
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    患者入院履歴マスタ−
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
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
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者所得情報編集サブ
           COPY    "CPORCSSYOTOKU.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "CPORCSSKJKEEP.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSSKJKEEP-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-SKJKEEP-HENKYAKU-AREA
           MOVE    ZERO            TO  LNK-SKJKEEP-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-SKJKEEP-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    主処理
           IF      LNK-SKJKEEP-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *     DISPLAY "LNK-SKJKEEP-RC = " LNK-SKJKEEP-RC
      *     PERFORM    VARYING IDX   FROM   1   BY  1
      *                UNTIL IDX   >   31
      *       DISPLAY "IDX = " IDX " LNK-SKJKEEP-KBN = "
      *                 LNK-SKJKEEP-KBN(IDX)
      *     END-PERFORM
      *     DISPLAY "LNK-SKJKEEP-SEI-NYUINYMD = "
      *              LNK-SKJKEEP-SEI-NYUINYMD
      *     DISPLAY "LNK-SKJKEEP-SEI-COMMENT-KBN = "
      *              LNK-SKJKEEP-SEI-COMMENT-KBN
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    算定年月チェック
           IF     (LNK-SKJKEEP-SANTEIYM     =  SPACE)      OR
                  (LNK-SKJKEEP-SANTEIYM    <=  "201603")
               MOVE    1                  TO  LNK-SKJKEEP-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    患者マスター検索
           INITIALIZE                         PTINF-REC
           MOVE    LNK-SKJKEEP-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    LNK-SKJKEEP-PTID        TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF              NOT =   ZERO
               MOVE    2                  TO  LNK-SKJKEEP-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    入院中のチェック（入院会計の保険組合せ検索）
           INITIALIZE                         NYUINACCT-REC
           MOVE    LNK-SKJKEEP-HOSPNUM     TO  NACCT-HOSPNUM
           MOVE    LNK-SKJKEEP-PTID        TO  NACCT-PTID
           MOVE    LNK-SKJKEEP-SANTEIYM    TO  NACCT-SRYYM
           MOVE    "5"                    TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key17"                TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT          NOT =   ZERO
               MOVE    3                  TO  LNK-SKJKEEP-RC
           ELSE
               MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE    "key17"            TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT         NOT =   ZERO
                    MOVE    3             TO  LNK-SKJKEEP-RC
               ELSE
                   PERFORM    VARYING IDX   FROM   1   BY  1
                      UNTIL IDX   >   31
                        IF  NACCT-DAY(IDX)  NOT =   ZERO
                            MOVE  NACCT-DAY(IDX)
                                          TO   WRK-HKNCOMBI-DAY(IDX)
                        END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key17"                TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *    月末日を取得
           INITIALIZE                         STS-AREA-DAY
           INITIALIZE                         LNK-DAY7-AREA
           MOVE    "71"                   TO  LNK-DAY7-IRAI
           MOVE    LNK-SKJKEEP-SANTEIYM
                                          TO  LNK-DAY7-YM
           CALL    "ORCSDAY"          USING   STS-AREA-DAY
                                              LNK-DAY7-AREA
           IF      STS-DAY-RC1            =   ZERO
               MOVE    LNK-DAY7-KOYOMI    TO  WRK-GETSUMATSU-YMD
           END-IF
      *
      *    精神病床長期入院の判定は病院だけなので、システム管理テーブル
      *    を判定
           MOVE    ZERO                TO  FLG-SEISIN-CHK
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-SKJKEEP-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    LNK-SKJKEEP-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   910-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF   FLG-SYSKANRI       =   ZERO
                  MOVE   SYSKANRI-REC  TO  SYS-1001-REC
                  IF     SYS-1001-HOSPSBT  =   1
                      MOVE   1         TO  FLG-SEISIN-CHK
                  END-IF
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *    入院履歴より２７年４月１日以前から２８年４月１まで継続して
      *    精神病床に入院しているか判定
           IF    FLG-SEISIN-CHK     =     1
               PERFORM    210-SEISIN-CHK1-SEC
           END-IF
      *
      *    精神病床の食事負担据え置きをシステム予約コード入力から判定
           IF    FLG-SEISIN-CHK     =     1
               PERFORM    210-SEISIN-CHK2-SEC
               IF     LNK-SKJKEEP-RC      =    5
                   GO  TO  200-MAIN-EXT
               END-IF
           END-IF
      *
      *    難病等の入院判定（診療行為で難病等に関わる診療が有る場合も対象）
           PERFORM    220-NANBYO-CHK-SEC
      *
      *    平成２９年９月まではサブルーチンでの患者所得情報の取得を行い、
      *    低所得者は返却値をクリアする
           IF   LNK-SKJKEEP-SANTEIYM  <=   "201709"
               INITIALIZE                          ORCSSYOTOKUAREA
               MOVE    LNK-SKJKEEP-HOSPNUM     TO  LNK-SYOTOKU-HOSPNUM
               MOVE    LNK-SKJKEEP-PTID        TO  LNK-SYOTOKU-PTID
               MOVE    LNK-SKJKEEP-SANTEIYM    TO  LNK-SYOTOKU-SRYYM
               MOVE    "1"                     TO  LNK-SYOTOKU-SKJCHKFLG
               CALL   "ORCSSYOTOKU"    USING    ORCSSYOTOKUAREA
                                                SPA-AREA
               IF    LNK-SYOTOKU-RC    =        ZERO
                  PERFORM   VARYING   IDX   FROM   1  BY  1
                            UNTIL     IDX   >    31
                    IF    LNK-SYOTOKU-SKJKBN(IDX)  NOT =  SPACE
                           MOVE    SPACE   TO  LNK-SKJKEEP-KBN(IDX)
                    END-IF
                  END-PERFORM
               END-IF
           END-IF
      *
      *    精神病床の減額があるか判定し、無ければ返却する入院開始日を
      *    クリアする
           PERFORM    VARYING   IDX   FROM  1  BY  1
             UNTIL    IDX    >   31
              IF     LNK-SKJKEEP-KBN(IDX)   =   "2"
                 MOVE   1     TO   FLG-SEISIN-SANTEI
              END-IF
           END-PERFORM
           IF      FLG-SEISIN-SANTEI   =   ZERO
                MOVE   SPACE           TO  LNK-SKJKEEP-SEI-NYUINYMD
           END-IF
      *
           IF       LNK-SKJKEEP-TBL   =    SPACE
               MOVE     4              TO   LNK-SKJKEEP-RC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    精神病床据え置き判定
      *  （２７年４月１日以前から２８年４月１まで継続して精神病床に
      *    入院しているか判定）
      *****************************************************************
       210-SEISIN-CHK1-SEC            SECTION.
      *
      *    事前に病棟情報（システム管理）から精神病棟の病棟番号を
      *    取得しておく
           MOVE    ZERO                TO  FLG-SEISIN-IGAI
           MOVE    ZERO                TO  IDX-SYS
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    LNK-SKJKEEP-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM   910-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL  (FLG-SYSKANRI   =   1)  OR
                               (IDX-SYS        =   300)
                 MOVE   SYSKANRI-REC   TO  SYS-5001-REC
                 IF   SYS-5001-BTU-SBT =   "02"
                    ADD    1           TO  IDX-SYS
                    MOVE   SYS-5001-KBNCD(1:2)
                                       TO  WRK-CHKBTU-NUM(IDX-SYS)
                    MOVE   SYS-5001-STYUKYMD
                                       TO  WRK-CHKBTU-STYUKYMD(IDX-SYS)
                    MOVE   SYS-5001-EDYUKYMD
                                       TO  WRK-CHKBTU-EDYUKYMD(IDX-SYS)
                 ELSE
                    MOVE   1           TO  FLG-SEISIN-IGAI
                 END-IF
                 MOVE    "tbl_syskanri" TO  MCP-TABLE
                 MOVE    "key5"         TO  MCP-PATHNAME
                 PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *     DISPLAY "WRK-CHKBTU-NUM(1) = " WRK-CHKBTU-NUM(1)
      *     DISPLAY "WRK-CHKBTU-NUM(2) = " WRK-CHKBTU-NUM(2)
      *     DISPLAY "WRK-CHKBTU-NUM(3) = " WRK-CHKBTU-NUM(3)
      *     DISPLAY "WRK-CHKBTU-NUM(4) = " WRK-CHKBTU-NUM(4)
      *     DISPLAY "WRK-CHKBTU-NUM(5) = " WRK-CHKBTU-NUM(5)
      *
      *    入院履歴より２７年４月１日以前から２８年４月１日まで継続して
      *    精神病床に入院しているか判定
           MOVE    ZERO                   TO  WRK-RRKNUM
           MOVE    ZERO                   TO  WRK-RRKNUM2
           INITIALIZE                         PTNYUINRRK-REC
           MOVE    LNK-SKJKEEP-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-SKJKEEP-PTID       TO  PTNYUINRRK-PTID
           MOVE    "20150401"             TO  PTNYUINRRK-NYUINYMD
           MOVE    "20160401"             TO  PTNYUINRRK-TAIINYMD
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key24"                TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key24"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
      *         DISPLAY    "========  FLG-PTNYUINRRK(1) = "
      *                               FLG-PTNYUINRRK
               MOVE    PTNYUINRRK-NYUINYMD       TO  WRK-SEI-NYUINYMD
               IF      FLG-PTNYUINRRK     =   ZERO
                   MOVE    PTNYUINRRK-RRKNUM     TO  WRK-RRKNUM
                   MOVE    PTNYUINRRK-NYUINYMD   TO  WRK-NYUINYMD
                   MOVE    PTNYUINRRK-TAIINYMD   TO  WRK-TAIINYMD
      *
                   INITIALIZE                        PTNYUINRRK-REC
                   MOVE    LNK-SKJKEEP-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
                   MOVE    LNK-SKJKEEP-PTID      TO  PTNYUINRRK-PTID
                   MOVE    WRK-RRKNUM            TO  PTNYUINRRK-RRKNUM
                   MOVE    "tbl_ptnyuinrrk"      TO  MCP-TABLE
                   MOVE    "key67"               TO  MCP-PATHNAME
                   PERFORM 910-PTNYUINRRK-SELECT-SEC
                   IF      FLG-PTNYUINRRK         =   ZERO
                       MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
                       MOVE    "key67"            TO  MCP-PATHNAME
                       PERFORM   910-PTNYUINRRK-READ-SEC
      *                 DISPLAY    "========  FLG-PTNYUINRRK(2) = "
      *                                       FLG-PTNYUINRRK
      **                 MOVE     SPACE             TO  WRK-MATCH
                       PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)  OR
                                         (WRK-MATCH        =  "NG")
      **                     MOVE     SPACE             TO  WRK-MATCH
      *                     DISPLAY "PTNYUINRRK-RRKNUM = "
      *                              PTNYUINRRK-RRKNUM
      *                     DISPLAY "PTNYUINRRK-TENNYUYMD = "
      *                              PTNYUINRRK-TENNYUYMD
      *                     DISPLAY "PTNYUINRRK-TENSTUYMD = "
      *                              PTNYUINRRK-TENSTUYMD
                           IF ((PTNYUINRRK-TENNYUYMD  <= "20150401") AND
                               (PTNYUINRRK-TENSTUYMD  >= "20150401"))
                                                                     OR
                              ((PTNYUINRRK-TENNYUYMD  >= "20150401") AND
                               (PTNYUINRRK-TENSTUYMD  <= "20160401"))
                                                                     OR
                              ((PTNYUINRRK-TENNYUYMD  <= "20160401") AND
                               (PTNYUINRRK-TENSTUYMD  >= "20160401"))
                               MOVE     SPACE             TO  WRK-MATCH
                               PERFORM    VARYING   IDX1  FROM  1  BY  1
                                 UNTIL   (IDX1      >     IDX-SYS)
                                  IF   WRK-CHKBTU-NUM(IDX1)  = 
                                                      PTNYUINRRK-BTUNUM
                                      MOVE   "OK"     TO   WRK-MATCH
                                  END-IF
                               END-PERFORM
                               IF    WRK-MATCH         =   SPACE
                                   MOVE   "NG"         TO  WRK-MATCH
                               END-IF
                           END-IF
                           MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
                           MOVE    "key67"            TO  MCP-PATHNAME
                           PERFORM 910-PTNYUINRRK-READ-SEC
                       END-PERFORM
                   END-IF
      *
      *            患者入院履歴クローズ
                   MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
                   MOVE    "key67"                 TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
               END-IF
           END-IF
      *
      *    患者入院履歴クローズ
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key24"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *     DISPLAY    "WRK-MATCH = " WRK-MATCH
      *
           IF        WRK-MATCH           =      "OK"
      *        据え置き区分のセット
               IF    WRK-TAIINYMD        =   "99999999"
                   PERFORM    VARYING   IDX1  FROM  1  BY  1
                     UNTIL   (IDX1      >     WRK-GETSUMATSU-DD)
                        MOVE   "2"      TO    LNK-SKJKEEP-KBN(IDX1)
                   END-PERFORM
               ELSE
                   IF    WRK-TAIIN-YYMM  <    LNK-SKJKEEP-SANTEIYM
                       CONTINUE
                   ELSE
                     IF  WRK-TAIIN-YYMM  >    LNK-SKJKEEP-SANTEIYM
                         PERFORM    VARYING   IDX1  FROM  1  BY  1
                            UNTIL   (IDX1      >     WRK-GETSUMATSU-DD)
                            MOVE   "2"      TO    LNK-SKJKEEP-KBN(IDX1)
                         END-PERFORM
                     ELSE
      *                  退院月＝算定月
                         PERFORM    VARYING   IDX1  FROM  1  BY  1
                            UNTIL   (IDX1      >     WRK-TAIIN-DD)
                            MOVE   "2"      TO    LNK-SKJKEEP-KBN(IDX1)
                         END-PERFORM
                     END-IF
                   END-IF
               END-IF
      *        精神入院年月日のセット
               MOVE   WRK-SEI-NYUINYMD  TO  LNK-SKJKEEP-SEI-NYUINYMD
      *
               MOVE    ZERO                  TO  FLG-KEIZOKU
               MOVE    ZERO                  TO  WRK-SHONUM
               INITIALIZE                        PTNYUINRRK-REC
               MOVE    LNK-SKJKEEP-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
               MOVE    LNK-SKJKEEP-PTID      TO  PTNYUINRRK-PTID
               MOVE    WRK-RRKNUM            TO  PTNYUINRRK-RRKNUM
               MOVE    "tbl_ptnyuinrrk"      TO  MCP-TABLE
               MOVE    "key69"               TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-SELECT-SEC
               IF      FLG-PTNYUINRRK         =   ZERO
                   MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
                   MOVE    "key69"            TO  MCP-PATHNAME
                   PERFORM   910-PTNYUINRRK-READ-SEC
                   MOVE     SPACE             TO  WRK-MATCH2
                   PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)  OR
                                     (WRK-MATCH2       =  "NG")
                       IF   PTNYUINRRK-TENNYUYMD  <= "20150401"
                               PERFORM    VARYING   IDX1  FROM  1  BY  1
                                 UNTIL   (IDX1      >     IDX-SYS)
                                  IF   WRK-CHKBTU-NUM(IDX1)  = 
                                                      PTNYUINRRK-BTUNUM
                                      MOVE   "OK"     TO   WRK-MATCH2
                                      MOVE    PTNYUINRRK-TENNYUYMD
                                           TO   WRK-SEI-NYUINYMD
                                                LNK-SKJKEEP-SEI-NYUINYMD
                                      MOVE    PTNYUINRRK-RRKNUM
                                                TO  WRK-RRKNUM2
                                      IF      PTNYUINRRK-SHOKAIKBN = "2"
                                        MOVE  1   TO  FLG-KEIZOKU
                                        MOVE  PTNYUINRRK-SHONUM
                                                  TO  WRK-SHONUM
                                      END-IF
                                  END-IF
                               END-PERFORM
                               IF    WRK-MATCH2        =   SPACE
                                   MOVE   "NG"         TO  WRK-MATCH
                               END-IF
                       END-IF
                       MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
                       MOVE    "key69"            TO  MCP-PATHNAME
                       PERFORM 910-PTNYUINRRK-READ-SEC
                   END-PERFORM
               END-IF
      *
      *        患者入院履歴クローズ
               MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
               MOVE    "key69"                 TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
      *
      *         DISPLAY  "FLG-KEIZOKU = " FLG-KEIZOKU
      *        精神病床の初回入院年月日のセット
               IF      FLG-KEIZOKU      =    1
                 INITIALIZE                        PTNYUINRRK-REC
                 MOVE    LNK-SKJKEEP-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
                 MOVE    LNK-SKJKEEP-PTID      TO  PTNYUINRRK-PTID
                 MOVE    WRK-SHONUM            TO  PTNYUINRRK-SHONUM
                 MOVE    "tbl_ptnyuinrrk"      TO  MCP-TABLE
                 MOVE    "key70"               TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-SELECT-SEC
                 IF      FLG-PTNYUINRRK         =   ZERO
                   MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
                   MOVE    "key70"            TO  MCP-PATHNAME
                   PERFORM   910-PTNYUINRRK-READ-SEC
                   MOVE     SPACE             TO  WRK-MATCH3
                   PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)  OR
                                     (WRK-MATCH3       =  "OK")
                       IF   PTNYUINRRK-TENNYUYMD  <= "20150401"
                           PERFORM    VARYING   IDX1  FROM  1  BY  1
                             UNTIL   (IDX1      >     IDX-SYS)
                              IF   WRK-CHKBTU-NUM(IDX1)  = 
                                                  PTNYUINRRK-BTUNUM
                                  MOVE   "OK"     TO   WRK-MATCH3
                                  MOVE    PTNYUINRRK-TENNYUYMD
                                       TO   WRK-SEI-NYUINYMD
                                            LNK-SKJKEEP-SEI-NYUINYMD
                                  MOVE    PTNYUINRRK-RRKNUM
                                                TO  WRK-RRKNUM2
                              END-IF
                           END-PERFORM
                       END-IF
                       MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
                       MOVE    "key70"            TO  MCP-PATHNAME
                       PERFORM 910-PTNYUINRRK-READ-SEC
                   END-PERFORM
                 END-IF
      *          患者入院履歴クローズ
                 MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
                 MOVE    "key70"                 TO  MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
               END-IF
      *
      *        歴作成された入院歴が継続入院中であれば対象履歴の入院年月日
      *        を返却
               IF     (FLG-SEISIN-IGAI            =     ZERO)    AND
                      (WRK-SEI-NYUINYMD          NOT =  SPACE)
                   INITIALIZE                        PTNYUINRRK-REC
                   MOVE    LNK-SKJKEEP-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
                   MOVE    LNK-SKJKEEP-PTID      TO  PTNYUINRRK-PTID
                   MOVE    WRK-RRKNUM2           TO  PTNYUINRRK-RRKNUM
                   MOVE    "tbl_ptnyuinrrk"      TO  MCP-TABLE
                   MOVE    "key4"                TO  MCP-PATHNAME
                   PERFORM 910-PTNYUINRRK-SELECT-SEC
                   IF      FLG-PTNYUINRRK         =   ZERO
                       MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
                       MOVE    "key4"            TO  MCP-PATHNAME
                       PERFORM   910-PTNYUINRRK-READ-SEC
                       IF   (FLG-PTNYUINRRK          =   ZERO)
      *
      *                    歴作成分も初歴を遡って入院日を返却
                           MOVE     PTNYUINRRK-SHONUM    TO  WRK-SHONUM
                           INITIALIZE                   PTNYUINRRK-REC
                           MOVE    LNK-SKJKEEP-HOSPNUM   TO
                                                     PTNYUINRRK-HOSPNUM
                           MOVE    LNK-SKJKEEP-PTID      TO
                                                     PTNYUINRRK-PTID
                           MOVE    WRK-SHONUM            TO
                                                     PTNYUINRRK-SHONUM
                           MOVE    "tbl_ptnyuinrrk"      TO MCP-TABLE
                           MOVE    "key71"               TO MCP-PATHNAME
                           PERFORM 910-PTNYUINRRK-SELECT-SEC
                           IF      FLG-PTNYUINRRK         =   ZERO
                              MOVE    "tbl_ptnyuinrrk"   TO MCP-TABLE
                              MOVE    "key71"            TO MCP-PATHNAME
                              PERFORM   910-PTNYUINRRK-READ-SEC
                              IF       FLG-PTNYUINRRK   =   ZERO
                                 MOVE    PTNYUINRRK-TENNYUYMD
                                         TO   WRK-SEI-NYUINYMD
                                              LNK-SKJKEEP-SEI-NYUINYMD
                              END-IF
                           END-IF
      *                   患者入院履歴クローズ
                          MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                          MOVE    "key71"              TO  MCP-PATHNAME
                          PERFORM   900-CLOSE-SEC
      *
      *
                       END-IF
                   END-IF
      *
      *            患者入院履歴クローズ
                   MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
                   MOVE    "key4"                  TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
               END-IF
      *
           END-IF
           .
       210-SEISIN-CHK1-EXT.
           EXIT.
      *
      *****************************************************************
      *    精神病床据え置き判定
      *  （システム予約コード入力から判定）
      *  （標準負担額経過措置（精神）入院年月日のコメント入力も取得）
      *****************************************************************
       210-SEISIN-CHK2-SEC            SECTION.
      *
      *    食事負担据え置き対象のシステム予約コード入力日を検索
           MOVE     ZERO                 TO   IDX-YYK
           INITIALIZE                         SRYACT-REC
           MOVE    LNK-SKJKEEP-HOSPNUM   TO   SRY-HOSPNUM
           MOVE    "1"                   TO   SRY-NYUGAIKBN
           MOVE    LNK-SKJKEEP-PTID      TO   SRY-PTID
           MOVE    "201604"              TO   SRY-SRYYM
           MOVE    CONS-SEISIN-ARI       TO   SRY-SRYCD(1)
                                              SRY-SRYCD(2)
                                              SRY-SRYCD(3)
                                              SRY-SRYCD(4)
                                              SRY-SRYCD(5)
           MOVE    "tbl_sryact"          TO   MCP-TABLE
           MOVE    "key21"               TO   MCP-PATHNAME
           PERFORM 910-SRYACT-SELECT-SEC
           IF      FLG-SRYACT            =    ZERO
               MOVE     "tbl_sryact"     TO   MCP-TABLE
               MOVE     "key21"          TO   MCP-PATHNAME
               PERFORM   910-SRYACT-READ-SEC
               PERFORM   UNTIL  (FLG-SRYACT   =   1)
      *
                  INITIALIZE                          SRYACCT-REC
                  MOVE    LNK-SKJKEEP-HOSPNUM     TO  ACCT-HOSPNUM
                  MOVE    LNK-SKJKEEP-PTID        TO  ACCT-PTID
                  MOVE    SRY-ZAINUM              TO  ACCT-ZAINUM
                  MOVE    "tbl_sryacct"           TO  MCP-TABLE
                  MOVE    "key57"                 TO  MCP-PATHNAME
                  PERFORM 910-SRYACCT-SELECT-SEC
                  IF      FLG-SRYACCT             =   ZERO
                     MOVE    "tbl_sryacct"        TO  MCP-TABLE
                     MOVE    "key57"              TO  MCP-PATHNAME
                     PERFORM   910-SRYACCT-READ-SEC
                     PERFORM   UNTIL   (FLG-SRYACCT    =   1)  OR
                                       (IDX-YYK        >=  21)
                        IF   ACCT-NYUGAIKBN   =     "1"
                            PERFORM    VARYING  IDX-ACCT  FROM  1  BY  1
                              UNTIL   (IDX-ACCT   >    31)   OR
                                      (IDX-YYK    >=   21)
                                IF   ACCT-DAY(1 IDX-ACCT)  NOT =  ZERO
                                   ADD    1       TO  IDX-YYK
                                   MOVE   ACCT-SRYYM
                                                   TO  WRK-SYSYYK-SRYYM
                                                             (IDX-YYK)
                                   MOVE   IDX-ACCT TO  WRK-SYSYYK-SRYDD
                                                             (IDX-YYK)
                                END-IF
                            END-PERFORM
                        END-IF
                        MOVE     "tbl_sryacct"      TO  MCP-TABLE
                        MOVE     "key57"            TO  MCP-PATHNAME
                        PERFORM   910-SRYACCT-READ-SEC
                    END-PERFORM
      *             診療会計クローズ
                    MOVE     "tbl_sryacct"   TO  MCP-TABLE
                    MOVE     "key57"          TO  MCP-PATHNAME
                    PERFORM   900-CLOSE-SEC
                  END-IF
                  MOVE     "tbl_sryact"     TO   MCP-TABLE
                  MOVE     "key21"          TO   MCP-PATHNAME
                  PERFORM   910-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *    診療行為クローズ
           MOVE     "tbl_sryact"     TO  MCP-TABLE
           MOVE     "key21"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(1) = " WRK-SYSYYK-SRYYMD(1)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(2) = " WRK-SYSYYK-SRYYMD(2)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(3) = " WRK-SYSYYK-SRYYMD(3)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(4) = " WRK-SYSYYK-SRYYMD(4)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(5) = " WRK-SYSYYK-SRYYMD(5)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(6) = " WRK-SYSYYK-SRYYMD(6)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(7) = " WRK-SYSYYK-SRYYMD(7)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(8) = " WRK-SYSYYK-SRYYMD(8)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(9) = " WRK-SYSYYK-SRYYMD(9)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(10) = " WRK-SYSYYK-SRYYMD(10)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(11) = " WRK-SYSYYK-SRYYMD(11)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(12) = " WRK-SYSYYK-SRYYMD(12)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(13) = " WRK-SYSYYK-SRYYMD(13)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(14) = " WRK-SYSYYK-SRYYMD(14)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(15) = " WRK-SYSYYK-SRYYMD(15)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(16) = " WRK-SYSYYK-SRYYMD(16)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(17) = " WRK-SYSYYK-SRYYMD(17)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(18) = " WRK-SYSYYK-SRYYMD(18)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(19) = " WRK-SYSYYK-SRYYMD(19)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(20) = " WRK-SYSYYK-SRYYMD(20)
      *     DISPLAY  "WRK-SYSYYK-SRYYMD(21) = " WRK-SYSYYK-SRYYMD(21)
      *
      *    システム予約コード入力が２０回を超えていないことをチェック
           IF     WRK-SYSYYK-SRYYMD(21)   NOT =   SPACE
               MOVE    5                  TO  LNK-SKJKEEP-RC
               GO  TO  210-SEISIN-CHK2-EXT
           END-IF
      *
           PERFORM    VARYING    IDX    FROM  1   BY  1
             UNTIL    (IDX    >   20)   OR
                      (WRK-SYSYYK-SRYYMD(IDX)  =  SPACE)
             INITIALIZE                         PTNYUINRRK-REC
             MOVE    LNK-SKJKEEP-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
             MOVE    LNK-SKJKEEP-PTID       TO  PTNYUINRRK-PTID
             MOVE    WRK-SYSYYK-SRYYMD(IDX) TO  PTNYUINRRK-NYUINYMD
                                                PTNYUINRRK-TAIINYMD
             MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
             MOVE    "key68"                TO  MCP-PATHNAME
             PERFORM 910-PTNYUINRRK-SELECT-SEC
             IF      FLG-PTNYUINRRK         =   ZERO
                MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                MOVE    "key68"             TO  MCP-PATHNAME
                PERFORM   910-PTNYUINRRK-READ-SEC
                IF       FLG-PTNYUINRRK     =   ZERO
                    MOVE     PTNYUINRRK-RRKNUM
                                            TO  WRK-SYSYYK-RRKNUM(IDX)
                    MOVE     PTNYUINRRK-NYUINYMD
                                            TO  WRK-SYSYYK-NYUINYMD(IDX)
                    MOVE     PTNYUINRRK-TAIINYMD
                                            TO  WRK-SYSYYK-TAIINYMD(IDX)
                END-IF
      *
      *         患者入院履歴クローズ
                MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
                MOVE    "key68"                 TO  MCP-PATHNAME
                PERFORM   900-CLOSE-SEC
             END-IF
           END-PERFORM
      *
      *     DISPLAY  "WRK-SYSYYK-RRKNUM(1) = " WRK-SYSYYK-RRKNUM(1)
      *     DISPLAY  "WRK-SYSYYK-NYUINYMD(1)= " WRK-SYSYYK-NYUINYMD(1)
      *     DISPLAY  "WRK-SYSYYK-TAIINYMD(1)= " WRK-SYSYYK-TAIINYMD(1)
      *     DISPLAY  "WRK-SYSYYK-RRKNUM(2) = " WRK-SYSYYK-RRKNUM(2)
      *     DISPLAY  "WRK-SYSYYK-NYUINYMD(2)= " WRK-SYSYYK-NYUINYMD(2)
      *     DISPLAY  "WRK-SYSYYK-TAIINYMD(2)= " WRK-SYSYYK-TAIINYMD(2)
      *     DISPLAY  "WRK-SYSYYK-RRKNUM(3) = " WRK-SYSYYK-RRKNUM(3)
      *     DISPLAY  "WRK-SYSYYK-NYUINYMD(3)= " WRK-SYSYYK-NYUINYMD(3)
      *     DISPLAY  "WRK-SYSYYK-TAIINYMD(3)= " WRK-SYSYYK-TAIINYMD(3)
      *
           PERFORM    VARYING    IDX    FROM  1   BY  1
             UNTIL    (IDX    >   20)   OR
                      (WRK-SYSYYK-RRKNUM(IDX)  =  ZERO)
                IF  (WRK-SYSYYK-NYUINYM(IDX) <=  LNK-SKJKEEP-SANTEIYM)
                AND (WRK-SYSYYK-TAIINYM(IDX) >=  LNK-SKJKEEP-SANTEIYM)
                  IF   WRK-SYSYYK-NYUINYM(IDX) <  LNK-SKJKEEP-SANTEIYM
                      MOVE    1      TO    IDX-ST
                  END-IF
                  IF   WRK-SYSYYK-NYUINYM(IDX) =  LNK-SKJKEEP-SANTEIYM
                      MOVE    WRK-SYSYYK-NYUINDD(IDX)    TO    IDX-ST
                  END-IF
                  IF   WRK-SYSYYK-TAIINYM(IDX) >  LNK-SKJKEEP-SANTEIYM
                      MOVE    WRK-GETSUMATSU-DD   TO    IDX-ED
                  END-IF
                  IF   WRK-SYSYYK-TAIINYM(IDX) =  LNK-SKJKEEP-SANTEIYM
                      MOVE    WRK-SYSYYK-TAIINDD(IDX)    TO    IDX-ED
                  END-IF
      *
                  PERFORM    VARYING    IDX1    FROM  IDX-ST   BY  1
                    UNTIL    (IDX1   >   IDX-ED)
                        MOVE   "2"      TO    LNK-SKJKEEP-KBN(IDX1)
                  END-PERFORM
                END-IF
           END-PERFORM
      *
      *
      *    標準負担額経過措置（精神）入院年月日：平成のコメント入力を検索
           MOVE     ZERO                 TO   IDX-YYK
           INITIALIZE                         SRYACT-REC
           MOVE    LNK-SKJKEEP-HOSPNUM   TO   SRY-HOSPNUM
           MOVE    "1"                   TO   SRY-NYUGAIKBN
           MOVE    LNK-SKJKEEP-PTID      TO   SRY-PTID
           MOVE    "201604"              TO   SRY-SRYYM
           MOVE    CONS-SEISIN-COMMENT-H TO   SRY-SRYCD(1)
                                              SRY-SRYCD(2)
                                              SRY-SRYCD(3)
                                              SRY-SRYCD(4)
                                              SRY-SRYCD(5)
           MOVE    "tbl_sryact"          TO   MCP-TABLE
           MOVE    "key21"               TO   MCP-PATHNAME
           PERFORM 910-SRYACT-SELECT-SEC
           IF      FLG-SRYACT            =    ZERO
               MOVE     "tbl_sryact"     TO   MCP-TABLE
               MOVE     "key21"          TO   MCP-PATHNAME
               PERFORM   910-SRYACT-READ-SEC
               IF    FLG-SRYACT          =    ZERO
      *
                   INITIALIZE                 PTCOM-REC
                   MOVE  SRY-HOSPNUM     TO   PTCOM-HOSPNUM
                   MOVE  SRY-PTID        TO   PTCOM-PTID
                   MOVE  SRY-ZAINUM      TO   PTCOM-ZAINUM
                   MOVE  CONS-SEISIN-COMMENT-H
                                         TO   PTCOM-SRYCD
                   MOVE  1               TO   PTCOM-RENNUM
                   MOVE  "tbl_ptcom"     TO   MCP-TABLE
                   MOVE  "key"           TO   MCP-PATHNAME
                   PERFORM 910-PTCOM-SELECT-SEC
                   IF     FLG-PTCOM      =    ZERO
                       MOVE  "tbl_ptcom" TO   MCP-TABLE
                       MOVE  "key"       TO   MCP-PATHNAME
                       PERFORM   910-PTCOM-READ-SEC
                       IF     FLG-PTCOM  =    ZERO
      *                    DISPLAY "PTCOM-SRYCD = " PTCOM-SRYCD
      *                    DISPLAY "PTCOM-INPUTCOMENT = " 
      *                                            PTCOM-INPUTCOMENT
                          INITIALIZE            STS-AREA-DAY
                          INITIALIZE            LNK-DAY1-AREA
                          MOVE    "12"          TO  LNK-DAY1-IRAI
                          MOVE    ZERO          TO  LNK-DAY1-YMD
                          MOVE    "4"              TO  LNK-DAY1-YMD(2:1)
                          MOVE    PTCOM-INPUTCHI1  TO  LNK-DAY1-YMD(3:2)
                          MOVE    PTCOM-INPUTCHI2  TO  LNK-DAY1-YMD(5:2)
                          MOVE    PTCOM-INPUTCHI3  TO  LNK-DAY1-YMD(7:2)
      *                   ２７年４月２日以降であれば昭和の元号に置き換え
                          IF    ( PTCOM-INPUTCHI1(1:2)   >=   "28") OR
                                ((PTCOM-INPUTCHI1(1:2)   =   "27")  AND
                                 (PTCOM-INPUTCHI2(1:2)   >=  "05")) OR
                                ((PTCOM-INPUTCHI1(1:2)   =   "27")  AND
                                 (PTCOM-INPUTCHI2(1:2)   =   "04")  AND
                                 (PTCOM-INPUTCHI3(1:2)   >=  "02"))
                               MOVE    "3"       TO  LNK-DAY1-YMD(2:1)
                          END-IF
                          CALL    "ORCSDAY"     USING   STS-AREA-DAY
                                                        LNK-DAY1-AREA
                          IF      STS-DAY-RC1      =   ZERO
      *                        DISPLAY   "LNK-DAY1-YMD-X = "
      *                                   LNK-DAY1-YMD-X
                              MOVE   LNK-DAY1-YMD-X
                                         TO  LNK-SKJKEEP-SEI-NYUINYMD
                              MOVE   "1" TO  LNK-SKJKEEP-SEI-COMMENT-KBN
                          END-IF
                        END-IF
                   END-IF
      *            患者コメントクローズ
                   MOVE     "tbl_ptcom"  TO  MCP-TABLE
                   MOVE     "key"        TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
               END-IF
           END-IF
      *    診療行為クローズ
           MOVE     "tbl_sryact"     TO  MCP-TABLE
           MOVE     "key21"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    標準負担額経過措置（精神）入院年月日：昭和のコメント入力を検索
           MOVE     ZERO                 TO   IDX-YYK
           INITIALIZE                         SRYACT-REC
           MOVE    LNK-SKJKEEP-HOSPNUM   TO   SRY-HOSPNUM
           MOVE    "1"                   TO   SRY-NYUGAIKBN
           MOVE    LNK-SKJKEEP-PTID      TO   SRY-PTID
           MOVE    "201604"              TO   SRY-SRYYM
           MOVE    CONS-SEISIN-COMMENT-S TO   SRY-SRYCD(1)
                                              SRY-SRYCD(2)
                                              SRY-SRYCD(3)
                                              SRY-SRYCD(4)
                                              SRY-SRYCD(5)
           MOVE    "tbl_sryact"          TO   MCP-TABLE
           MOVE    "key21"               TO   MCP-PATHNAME
           PERFORM 910-SRYACT-SELECT-SEC
           IF      FLG-SRYACT            =    ZERO
               MOVE     "tbl_sryact"     TO   MCP-TABLE
               MOVE     "key21"          TO   MCP-PATHNAME
               PERFORM   910-SRYACT-READ-SEC
               IF    FLG-SRYACT          =    ZERO
      *
                   INITIALIZE                 PTCOM-REC
                   MOVE  SRY-HOSPNUM     TO   PTCOM-HOSPNUM
                   MOVE  SRY-PTID        TO   PTCOM-PTID
                   MOVE  SRY-ZAINUM      TO   PTCOM-ZAINUM
                   MOVE  CONS-SEISIN-COMMENT-S
                                         TO   PTCOM-SRYCD
                   MOVE  1               TO   PTCOM-RENNUM
                   MOVE  "tbl_ptcom"     TO   MCP-TABLE
                   MOVE  "key"           TO   MCP-PATHNAME
                   PERFORM 910-PTCOM-SELECT-SEC
                   IF     FLG-PTCOM      =    ZERO
                       MOVE  "tbl_ptcom" TO   MCP-TABLE
                       MOVE  "key"       TO   MCP-PATHNAME
                       PERFORM   910-PTCOM-READ-SEC
                       IF     FLG-PTCOM  =    ZERO
      *                    DISPLAY "PTCOM-SRYCD = " PTCOM-SRYCD
      *                    DISPLAY "PTCOM-INPUTCOMENT = " 
      *                                            PTCOM-INPUTCOMENT
                          INITIALIZE            STS-AREA-DAY
                          INITIALIZE            LNK-DAY1-AREA
                          MOVE    "12"          TO  LNK-DAY1-IRAI
                          MOVE    ZERO          TO  LNK-DAY1-YMD
                          MOVE    "3"              TO  LNK-DAY1-YMD(2:1)
                          MOVE    PTCOM-INPUTCHI1  TO  LNK-DAY1-YMD(3:2)
                          MOVE    PTCOM-INPUTCHI2  TO  LNK-DAY1-YMD(5:2)
                          MOVE    PTCOM-INPUTCHI3  TO  LNK-DAY1-YMD(7:2)
                          CALL    "ORCSDAY"     USING   STS-AREA-DAY
                                                        LNK-DAY1-AREA
                          IF      STS-DAY-RC1      =   ZERO
      *                        DISPLAY   "LNK-DAY1-YMD-X = "
      *                                   LNK-DAY1-YMD-X
                              MOVE   LNK-DAY1-YMD-X
                                         TO  LNK-SKJKEEP-SEI-NYUINYMD
                              MOVE   "1" TO  LNK-SKJKEEP-SEI-COMMENT-KBN
                          END-IF
                        END-IF
                   END-IF
      *            患者コメントクローズ
                   MOVE     "tbl_ptcom"  TO  MCP-TABLE
                   MOVE     "key"        TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
               END-IF
           END-IF
      *    診療行為クローズ
           MOVE     "tbl_sryact"     TO  MCP-TABLE
           MOVE     "key21"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
           .
       210-SEISIN-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    難病等公費診療を判定
      *****************************************************************
       220-NANBYO-CHK-SEC            SECTION.
      *
      *    事前に該当患者が算定月に難病公費又は小児慢性特定疾病公費を
      *    持つ患者か判定を行う
           MOVE    ZERO                    TO  FLG-NANBYO
           MOVE    ZERO                    TO  IDX-NANBYO
           MOVE    ZERO                    TO  IDX-HKNCOMBI
      *
      *    難病
           INITIALIZE                          PTKOHINF-REC
           MOVE    LNK-SKJKEEP-HOSPNUM     TO  PTKOH-HOSPNUM
           MOVE    LNK-SKJKEEP-PTID        TO  PTKOH-PTID
           MOVE    "054"                   TO  PTKOH-KOHNUM
           MOVE    LNK-SKJKEEP-SANTEIYM    TO  PTKOH-TEKSTYMD(1:6)
           MOVE    "31"                    TO  PTKOH-TEKSTYMD(7:2)
           MOVE    LNK-SKJKEEP-SANTEIYM    TO  PTKOH-TEKEDYMD(1:6)
           MOVE    "01"                    TO  PTKOH-TEKEDYMD(7:2)
           MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
           MOVE    "key6"                  TO  MCP-PATHNAME
           PERFORM 910-PTKOHINF-SELECT-SEC
           IF      FLG-PTKOHINF            =   ZERO
               MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
               MOVE    "key6"               TO  MCP-PATHNAME
               PERFORM   910-PTKOHINF-READ-SEC
               PERFORM   UNTIL   (FLG-PTKOHINF   =   1)  OR
                                 (IDX-NANBYO     =   10)
                   MOVE     1               TO  FLG-NANBYO
                   ADD      1               TO  IDX-NANBYO
                   MOVE     PTKOH-KOHID     TO  WRK-NANBYO-KOHID
                                                     (IDX-NANBYO)
                   MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                   MOVE    "key6"           TO  MCP-PATHNAME
                   PERFORM 910-PTKOHINF-READ-SEC
               END-PERFORM
           END-IF
      *    患者公費情報クローズ
           MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
           MOVE    "key6"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *     DISPLAY "WRK-NANBYO-KOHID(1) = " WRK-NANBYO-KOHID(1)
      *     DISPLAY "WRK-NANBYO-KOHID(2) = " WRK-NANBYO-KOHID(2)
      *     DISPLAY "WRK-NANBYO-KOHID(3) = " WRK-NANBYO-KOHID(3)
      *     DISPLAY "WRK-NANBYO-KOHID(4) = " WRK-NANBYO-KOHID(4)
      *     DISPLAY "WRK-NANBYO-KOHID(5) = " WRK-NANBYO-KOHID(5)
      *
      *    小児慢性特定疾病
           INITIALIZE                          PTKOHINF-REC
           MOVE    LNK-SKJKEEP-HOSPNUM     TO  PTKOH-HOSPNUM
           MOVE    LNK-SKJKEEP-PTID        TO  PTKOH-PTID
           MOVE    "052"                   TO  PTKOH-KOHNUM
           MOVE    LNK-SKJKEEP-SANTEIYM    TO  PTKOH-TEKSTYMD(1:6)
           MOVE    "31"                    TO  PTKOH-TEKSTYMD(7:2)
           MOVE    LNK-SKJKEEP-SANTEIYM    TO  PTKOH-TEKEDYMD(1:6)
           MOVE    "01"                    TO  PTKOH-TEKEDYMD(7:2)
           MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
           MOVE    "key6"                  TO  MCP-PATHNAME
           PERFORM 910-PTKOHINF-SELECT-SEC
           IF      FLG-PTKOHINF            =   ZERO
               MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
               MOVE    "key6"               TO  MCP-PATHNAME
               PERFORM   910-PTKOHINF-READ-SEC
               PERFORM   UNTIL   (FLG-PTKOHINF   =   1)  OR
                                 (IDX-NANBYO     =   10)
                   MOVE     1               TO  FLG-NANBYO
                   ADD      1               TO  IDX-NANBYO
                   MOVE     PTKOH-KOHID     TO  WRK-NANBYO-KOHID
                                                     (IDX-NANBYO)
                   MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                   MOVE    "key6"           TO  MCP-PATHNAME
                   PERFORM 910-PTKOHINF-READ-SEC
               END-PERFORM
           END-IF
      *    患者公費情報クローズ
           MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
           MOVE    "key6"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    食事負担据え置き区分のセット
           IF        FLG-NANBYO      =     1
      *
      *      難病等食事負担据え置きのシステム予約コード入力日を検索
      *      据え置き対象
             MOVE     ZERO                 TO   IDX-YYK
             INITIALIZE                         SRYACT-REC
             MOVE    LNK-SKJKEEP-HOSPNUM   TO   SRY-HOSPNUM
             MOVE    "1"                   TO   SRY-NYUGAIKBN
             MOVE    LNK-SKJKEEP-PTID      TO   SRY-PTID
             MOVE    LNK-SKJKEEP-SANTEIYM  TO   SRY-SRYYM
             MOVE    CONS-NANBYO-ARI       TO   SRY-SRYCD(1)
                                                SRY-SRYCD(2)
                                                SRY-SRYCD(3)
                                                SRY-SRYCD(4)
                                                SRY-SRYCD(5)
             MOVE    "tbl_sryact"          TO   MCP-TABLE
             MOVE    "key7"                TO   MCP-PATHNAME
             PERFORM 910-SRYACT-SELECT-SEC
             IF      FLG-SRYACT            =    ZERO
                 MOVE     "tbl_sryact"     TO   MCP-TABLE
                 MOVE     "key7"           TO   MCP-PATHNAME
                 PERFORM   910-SRYACT-READ-SEC
                 PERFORM   UNTIL  (FLG-SRYACT   =   1)
      *
                    INITIALIZE                          SRYACCT-REC
                    MOVE    LNK-SKJKEEP-HOSPNUM     TO  ACCT-HOSPNUM
                    MOVE    LNK-SKJKEEP-PTID        TO  ACCT-PTID
                    MOVE    SRY-ZAINUM              TO  ACCT-ZAINUM
                    MOVE    "tbl_sryacct"           TO  MCP-TABLE
                    MOVE    "key57"                 TO  MCP-PATHNAME
                    PERFORM 910-SRYACCT-SELECT-SEC
                    IF      FLG-SRYACCT             =   ZERO
                       MOVE    "tbl_sryacct"        TO  MCP-TABLE
                       MOVE    "key57"              TO  MCP-PATHNAME
                       PERFORM   910-SRYACCT-READ-SEC
                       PERFORM   UNTIL   FLG-SRYACCT    =   1
                          IF   ACCT-NYUGAIKBN   =     "1"
                              PERFORM   VARYING  IDX-ACCT FROM  1  BY  1
                                UNTIL   IDX-ACCT   >  31
                                  IF   ACCT-DAY(1 IDX-ACCT)  NOT =  ZERO
                                     MOVE    "1"       TO
                                                    WRK-NANBYO-INPUT-OK
                                                             (IDX-ACCT)
                                     MOVE    "1"       TO
                                                  LNK-SKJKEEP-NANBYOKBN
                                                             (IDX-ACCT)
                                  END-IF
                              END-PERFORM
                          END-IF
                          MOVE     "tbl_sryacct"      TO  MCP-TABLE
                          MOVE     "key57"            TO  MCP-PATHNAME
                          PERFORM   910-SRYACCT-READ-SEC
                      END-PERFORM
      *               診療会計クローズ
                      MOVE     "tbl_sryacct"   TO  MCP-TABLE
                      MOVE     "key57"          TO  MCP-PATHNAME
                      PERFORM   900-CLOSE-SEC
                    END-IF
                    MOVE     "tbl_sryact"     TO   MCP-TABLE
                    MOVE     "key7"           TO   MCP-PATHNAME
                    PERFORM   910-SRYACT-READ-SEC
                 END-PERFORM
             END-IF
      *      診療行為クローズ
             MOVE     "tbl_sryact"     TO  MCP-TABLE
             MOVE     "key7"           TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
      *      難病等食事負担据え置き（急性増悪による転棟）のシステム予約
      *      コード入力日を検索据え置き対象
             MOVE     ZERO                 TO   IDX-YYK
             INITIALIZE                         SRYACT-REC
             MOVE    LNK-SKJKEEP-HOSPNUM   TO   SRY-HOSPNUM
             MOVE    "1"                   TO   SRY-NYUGAIKBN
             MOVE    LNK-SKJKEEP-PTID      TO   SRY-PTID
             MOVE    LNK-SKJKEEP-SANTEIYM  TO   SRY-SRYYM
             MOVE    CONS-NANBYO-ARI2      TO   SRY-SRYCD(1)
                                                SRY-SRYCD(2)
                                                SRY-SRYCD(3)
                                                SRY-SRYCD(4)
                                                SRY-SRYCD(5)
             MOVE    "tbl_sryact"          TO   MCP-TABLE
             MOVE    "key7"                TO   MCP-PATHNAME
             PERFORM 910-SRYACT-SELECT-SEC
             IF      FLG-SRYACT            =    ZERO
                 MOVE     "tbl_sryact"     TO   MCP-TABLE
                 MOVE     "key7"           TO   MCP-PATHNAME
                 PERFORM   910-SRYACT-READ-SEC
                 PERFORM   UNTIL  (FLG-SRYACT   =   1)
      *
                    INITIALIZE                          SRYACCT-REC
                    MOVE    LNK-SKJKEEP-HOSPNUM     TO  ACCT-HOSPNUM
                    MOVE    LNK-SKJKEEP-PTID        TO  ACCT-PTID
                    MOVE    SRY-ZAINUM              TO  ACCT-ZAINUM
                    MOVE    "tbl_sryacct"           TO  MCP-TABLE
                    MOVE    "key57"                 TO  MCP-PATHNAME
                    PERFORM 910-SRYACCT-SELECT-SEC
                    IF      FLG-SRYACCT             =   ZERO
                       MOVE    "tbl_sryacct"        TO  MCP-TABLE
                       MOVE    "key57"              TO  MCP-PATHNAME
                       PERFORM   910-SRYACCT-READ-SEC
                       PERFORM   UNTIL   FLG-SRYACCT    =   1
                          IF   ACCT-NYUGAIKBN   =     "1"
                              PERFORM   VARYING  IDX-ACCT FROM  1  BY  1
                                UNTIL   IDX-ACCT   >  31
                                  IF   ACCT-DAY(1 IDX-ACCT)  NOT =  ZERO
                                     MOVE    "1"       TO
                                                    WRK-NANBYO-INPUT-OK
                                                             (IDX-ACCT)
                                     MOVE    "2"       TO
                                                  LNK-SKJKEEP-NANBYOKBN
                                                             (IDX-ACCT)
                                  END-IF
                              END-PERFORM
                          END-IF
                          MOVE     "tbl_sryacct"      TO  MCP-TABLE
                          MOVE     "key57"            TO  MCP-PATHNAME
                          PERFORM   910-SRYACCT-READ-SEC
                      END-PERFORM
      *               診療会計クローズ
                      MOVE     "tbl_sryacct"   TO  MCP-TABLE
                      MOVE     "key57"          TO  MCP-PATHNAME
                      PERFORM   900-CLOSE-SEC
                    END-IF
                    MOVE     "tbl_sryact"     TO   MCP-TABLE
                    MOVE     "key7"           TO   MCP-PATHNAME
                    PERFORM   910-SRYACT-READ-SEC
                 END-PERFORM
             END-IF
      *      診療行為クローズ
             MOVE     "tbl_sryact"     TO  MCP-TABLE
             MOVE     "key7"           TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
      *      難病等食事負担据え置きのシステム予約コード入力日を検索
      *      据え置き対象外
             MOVE     ZERO                 TO   IDX-YYK
             INITIALIZE                         SRYACT-REC
             MOVE    LNK-SKJKEEP-HOSPNUM   TO   SRY-HOSPNUM
             MOVE    "1"                   TO   SRY-NYUGAIKBN
             MOVE    LNK-SKJKEEP-PTID      TO   SRY-PTID
             MOVE    LNK-SKJKEEP-SANTEIYM  TO   SRY-SRYYM
             MOVE    CONS-NANBYO-NASI      TO   SRY-SRYCD(1)
                                                SRY-SRYCD(2)
                                                SRY-SRYCD(3)
                                                SRY-SRYCD(4)
                                                SRY-SRYCD(5)
             MOVE    "tbl_sryact"          TO   MCP-TABLE
             MOVE    "key7"                TO   MCP-PATHNAME
             PERFORM 910-SRYACT-SELECT-SEC
             IF      FLG-SRYACT            =    ZERO
                 MOVE     "tbl_sryact"     TO   MCP-TABLE
                 MOVE     "key7"           TO   MCP-PATHNAME
                 PERFORM   910-SRYACT-READ-SEC
                 PERFORM   UNTIL  (FLG-SRYACT   =   1)
      *
                    INITIALIZE                          SRYACCT-REC
                    MOVE    LNK-SKJKEEP-HOSPNUM     TO  ACCT-HOSPNUM
                    MOVE    LNK-SKJKEEP-PTID        TO  ACCT-PTID
                    MOVE    SRY-ZAINUM              TO  ACCT-ZAINUM
                    MOVE    "tbl_sryacct"           TO  MCP-TABLE
                    MOVE    "key57"                 TO  MCP-PATHNAME
                    PERFORM 910-SRYACCT-SELECT-SEC
                    IF      FLG-SRYACCT             =   ZERO
                       MOVE    "tbl_sryacct"        TO  MCP-TABLE
                       MOVE    "key57"              TO  MCP-PATHNAME
                       PERFORM   910-SRYACCT-READ-SEC
                       PERFORM   UNTIL   FLG-SRYACCT    =   1
                          IF   ACCT-NYUGAIKBN   =     "1"
                              PERFORM   VARYING  IDX-ACCT FROM  1  BY  1
                                UNTIL   IDX-ACCT   >  31
                                  IF   ACCT-DAY(1 IDX-ACCT)  NOT =  ZERO
                                     MOVE    "1"       TO
                                                    WRK-NANBYO-INPUT-NG
                                                             (IDX-ACCT)
                                  END-IF
                              END-PERFORM
                          END-IF
                          MOVE     "tbl_sryacct"      TO  MCP-TABLE
                          MOVE     "key57"            TO  MCP-PATHNAME
                          PERFORM   910-SRYACCT-READ-SEC
                      END-PERFORM
      *               診療会計クローズ
                      MOVE     "tbl_sryacct"   TO  MCP-TABLE
                      MOVE     "key57"          TO  MCP-PATHNAME
                      PERFORM   900-CLOSE-SEC
                    END-IF
                    MOVE     "tbl_sryact"     TO   MCP-TABLE
                    MOVE     "key7"           TO   MCP-PATHNAME
                    PERFORM   910-SRYACT-READ-SEC
                 END-PERFORM
             END-IF
      *      診療行為クローズ
             MOVE     "tbl_sryact"     TO  MCP-TABLE
             MOVE     "key7"           TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
      *       PERFORM   VARYING IDX FROM 1 BY 1
      *         UNTIL   IDX > 31
      *          DISPLAY "IDX = " IDX
      *          DISPLAY "WRK-NANBYO-INPUT-OK= " WRK-NANBYO-INPUT-OK(IDX)
      *          DISPLAY "WRK-NANBYO-INPUT-NG= " WRK-NANBYO-INPUT-NG(IDX)
      *       END-PERFORM
      *
      *      対象となる保険組合せ番号を検索
             PERFORM    VARYING   IDX   FROM   1   BY   1
               UNTIL   (IDX    >    10)    OR
                       (WRK-NANBYO-KOHID(IDX)  =  ZERO)
                 INITIALIZE                          HKNCOMBI-REC
                 MOVE    LNK-SKJKEEP-HOSPNUM     TO  COMB-HOSPNUM
                 MOVE    LNK-SKJKEEP-PTID        TO  COMB-PTID
                 MOVE    WRK-NANBYO-KOHID(IDX)   TO  COMB-KOH1ID
                                                     COMB-KOH2ID
                                                     COMB-KOH3ID
                                                     COMB-KOH4ID
                 MOVE    "tbl_hkncombi"          TO  MCP-TABLE
                 MOVE    "key4"                  TO  MCP-PATHNAME
                 PERFORM 910-HKNCOMBI-SELECT-SEC
                 IF      FLG-HKNCOMBI            =   ZERO
                     MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                     MOVE    "key4"              TO  MCP-PATHNAME
                     PERFORM   910-HKNCOMBI-READ-SEC
                     PERFORM    UNTIL   (FLG-HKNCOMBI   =   1)  OR
                                        (IDX-HKNCOMBI   =   10)
                       IF  (COMB-TEKSTYMD(1:6) <= LNK-SKJKEEP-SANTEIYM)
                       AND (COMB-TEKEDYMD(1:6) >= LNK-SKJKEEP-SANTEIYM)
                           ADD      1                TO  IDX-HKNCOMBI
                           MOVE COMB-HKNCOMBINUM TO  WRK-NANBYO-HKNCOMBI
                                                     (IDX-HKNCOMBI)
                       END-IF
                       MOVE    "tbl_hkncombi"    TO  MCP-TABLE
                       MOVE    "key4"            TO  MCP-PATHNAME
                       PERFORM   910-HKNCOMBI-READ-SEC
                     END-PERFORM
                 END-IF
      *          保険組合せクローズ
                 MOVE    "tbl_hkncombi"    TO  MCP-TABLE
                 MOVE    "key4"            TO  MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
             END-PERFORM
      *
      *      入院会計テーブル（初期処理で参照した保険組合せから）
             PERFORM    VARYING IDX1   FROM   1   BY  1
               UNTIL    IDX1   >   31
                 IF  WRK-HKNCOMBI-DAY(IDX1)  NOT =   ZERO
                    INITIALIZE                    HKNCOMBI-REC
                    MOVE   LNK-SKJKEEP-HOSPNUM    TO  COMB-HOSPNUM
                    MOVE   LNK-SKJKEEP-PTID       TO  COMB-PTID
                    MOVE   WRK-HKNCOMBI-DAY(IDX1) TO  COMB-HKNCOMBINUM
                    MOVE   "tbl_hkncombi"         TO  MCP-TABLE
                    MOVE    "key"                 TO  MCP-PATHNAME
                    PERFORM 910-HKNCOMBI-SELECT-SEC
                    IF   FLG-HKNCOMBI       =   ZERO
                         MOVE  "tbl_hkncombi"     TO  MCP-TABLE
                         MOVE  "key"              TO  MCP-PATHNAME
                         PERFORM   910-HKNCOMBI-READ-SEC
                         IF  FLG-HKNCOMBI         =  ZERO
                             IF  (COMB-KOHHKNNUM(1) = "054")  OR
                                 (COMB-KOHHKNNUM(2) = "054")  OR
                                 (COMB-KOHHKNNUM(3) = "054")  OR
                                 (COMB-KOHHKNNUM(4) = "054")
                                IF  WRK-NANBYO-INPUT-NG(IDX1)  =  SPACE
                                  MOVE   "1"  TO   LNK-SKJKEEP-KBN(IDX1)
                                END-IF
                             END-IF
                             IF  (COMB-KOHHKNNUM(1) = "052")  OR
                                 (COMB-KOHHKNNUM(2) = "052")  OR
                                 (COMB-KOHHKNNUM(3) = "052")  OR
                                 (COMB-KOHHKNNUM(4) = "052")
                                IF  WRK-NANBYO-INPUT-NG(IDX1)  =  SPACE
                                  MOVE   "1"  TO   LNK-SKJKEEP-KBN(IDX1)
                                END-IF
                             END-IF
                         END-IF
                    END-IF
      *             保険組合せクローズ
                    MOVE    "tbl_hkncombi"  TO  MCP-TABLE
                    MOVE    "key"           TO  MCP-PATHNAME
                    PERFORM   900-CLOSE-SEC
                 END-IF
             END-PERFORM
      *
      *       DISPLAY "WRK-NANBYO-HKNCOMBI(1) = " WRK-NANBYO-HKNCOMBI(1)
      *       DISPLAY "WRK-NANBYO-HKNCOMBI(2) = " WRK-NANBYO-HKNCOMBI(2)
      *       DISPLAY "WRK-NANBYO-HKNCOMBI(3) = " WRK-NANBYO-HKNCOMBI(3)
      *       DISPLAY "WRK-NANBYO-HKNCOMBI(4) = " WRK-NANBYO-HKNCOMBI(4)
      *       DISPLAY "WRK-NANBYO-HKNCOMBI(5) = " WRK-NANBYO-HKNCOMBI(5)
      *      診療会計テーブル（包括入力分も対象）
             INITIALIZE                          SRYACCT-REC
             MOVE    LNK-SKJKEEP-HOSPNUM     TO  ACCT-HOSPNUM
             MOVE    "1"                     TO  ACCT-NYUGAIKBN
             MOVE    LNK-SKJKEEP-PTID        TO  ACCT-PTID
             MOVE    LNK-SKJKEEP-SANTEIYM    TO  ACCT-SRYYM
             MOVE    "tbl_sryacct"           TO  MCP-TABLE
             MOVE    "key68"                 TO  MCP-PATHNAME
             PERFORM 910-SRYACCT-SELECT-SEC
             IF      FLG-SRYACCT             =   ZERO
                 MOVE    "tbl_sryacct"       TO  MCP-TABLE
                 MOVE    "key68"             TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
                 PERFORM   UNTIL   (FLG-SRYACCT    =   1)
      *
                   IF    ACCT-HKNCOMBI   =   WRK-NANBYO-HKNCOMBI(1) OR
                                             WRK-NANBYO-HKNCOMBI(2) OR
                                             WRK-NANBYO-HKNCOMBI(3) OR
                                             WRK-NANBYO-HKNCOMBI(4) OR
                                             WRK-NANBYO-HKNCOMBI(5) OR
                                             WRK-NANBYO-HKNCOMBI(6) OR
                                             WRK-NANBYO-HKNCOMBI(7) OR
                                             WRK-NANBYO-HKNCOMBI(8) OR
                                             WRK-NANBYO-HKNCOMBI(9) OR
                                             WRK-NANBYO-HKNCOMBI(10)
                      IF   ACCT-SRYKBN   =   "95"  OR  "96"
                          CONTINUE
                      ELSE
                        PERFORM    VARYING   IDX   FROM   1   BY   1
                          UNTIL    IDX    >    31
                              IF   ACCT-DAY(1 IDX)  NOT =   ZERO
                                IF  WRK-NANBYO-INPUT-NG(IDX)  =  SPACE
                                  MOVE   "1"  TO   LNK-SKJKEEP-KBN(IDX)
                                END-IF
                              END-IF
                        END-PERFORM
                      END-IF
                   END-IF
      *
                   MOVE     "tbl_sryacct"      TO  MCP-TABLE
                   MOVE     "key68"            TO  MCP-PATHNAME
                   PERFORM   910-SRYACCT-READ-SEC
                 END-PERFORM
             END-IF
      *      診療会計クローズ
             MOVE     "tbl_sryacct"    TO  MCP-TABLE
             MOVE     "key68"          TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
      *      システム予約で難病等の食事負担据え置きを入力された場合の
      *      強制セット
             PERFORM    VARYING IDX1   FROM   1   BY  1
               UNTIL    IDX1   >   31
                 IF  WRK-NANBYO-INPUT-OK(IDX1)  =  "1"
                    MOVE   "1"  TO   LNK-SKJKEEP-KBN(IDX1)
                 END-IF
             END-PERFORM
      *
           END-IF
      *
           .
       220-NANBYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為取得
      *****************************************************************
       210-NYUINACT-GET-SEC            SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM   910-NYUINACT-READ-SEC
           END-IF
      *
      *    入院行為クローズ
           MOVE     "tbl_nyuinact"     TO  MCP-TABLE
           MOVE     "key9"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       210-NYUINACT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター検索
      *****************************************************************
       910-SYSKANRI-SELECT-SEC        SECTION.
      *
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SYSKANRI
           ELSE
               MOVE    1                  TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SYSKANRI-REC
               MOVE    ZERO               TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                     SYSKANRI-REC
               MOVE    1                  TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター検索
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACCT-REC
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                     NYUINACCT-REC
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為マスター検索
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
               MOVE    1                  TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為マスター読込
      *****************************************************************
       910-NYUINACT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター検索
      *****************************************************************
       910-SRYACCT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       910-SRYACCT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SRYACCT-REC
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               INITIALIZE                     SRYACCT-REC
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター検索
      *****************************************************************
       910-SRYACT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SRYACT
           ELSE
               MOVE    1                  TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       910-SRYACT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター検索
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-HKNCOMBI
           ELSE
               MOVE    1                  TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報マスター検索
      *****************************************************************
       910-PTKOHINF-SELECT-SEC        SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTKOHINF
           ELSE
               MOVE    1                  TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報マスター読込
      *****************************************************************
       910-PTKOHINF-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               MOVE    ZERO                TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                      PTKOHINF-REC
               MOVE    1                   TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメント検索
      *****************************************************************
       910-PTCOM-SELECT-SEC        SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTCOM
           ELSE
               MOVE    1               TO  FLG-PTCOM
           END-IF
      *
           .
       910-PTCOM-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメント読込
      *****************************************************************
       910-PTCOM-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTCOM-REC
               MOVE    ZERO               TO  FLG-PTCOM
           ELSE
               INITIALIZE                     PTCOM-REC
               MOVE    1                  TO  FLG-PTCOM
           END-IF
      *
           .
       910-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込（主キー）
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  TNS-TENSU-REC
                   MOVE    ZERO        TO  FLG-TENSU
               ELSE
                   MOVE    1           TO  FLG-TENSU
                   INITIALIZE              TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
      *
       900-CLOSE-EXT.
           EXIT.
