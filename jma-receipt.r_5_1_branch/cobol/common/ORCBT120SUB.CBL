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
       PROGRAM-ID.                 ORCBT120SUB.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 日次統計用入院情報取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/12/15    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-小豆沢  07/05/31  グループ診療対応
      *  04.01.01    NACL-小豆沢  07/11/16  公務災害、公害の種別判定追加
      *平成２０年４月改定対応
      *  04.02.00    NACL-小豆沢  08/04/15  レセプト種別対応
      *  04.02.01    NACL-藤原    08/04/23  後期高齢者の船員保険を本人とする
      *
      *  04.04.01    NACL-藤原    09/02/26  平成２０年４月以降に７５歳到
      *                                     達時に海外に住所を有する場合
      *                                     の対応
      *  04.04.02    NACL-藤原    10/08/23  後期高齢非該当対応
      *                                     （高齢受給者票がない場合）
      * 
      *  04.05.00    NACL-小豆沢  10/01/07  治験のレセプト種別対応
      *
      *  04.07.01    NACL-藤原    14/03/30  平成２６年４月改正対応
      *  04.07.02    NACL-藤原    14/08/18  ７５歳以上の海外居住者２割対応
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
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-PTHKNINF                PIC 9(01).
           03  FLG-PTRSIINF                PIC 9(01).
           03  FLG-PTKOHINF                PIC 9(01).
           03  FLG-NYUIN-HIT1              PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX2                        PIC 9(04).
           03  SYU-IDX                     PIC 9(04).
           03  COMB-IDX                    PIC 9(04).
           03  IDX-SKJ-TAIHI               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
      *    算定日退避
           03  WRK-SANTEIYMD.
               05  WRK-SANTEIYM.
                   07  WRK-SANTEIYY            PIC 9(04).
                   07  WRK-SANTEIMM            PIC 9(02).
               05  WRK-SANTEIDD                PIC 9(02).
      *
      *    前日算定日
           03  WRK-MAE-SANTEIYMD.
               05  WRK-MAE-SANTEIYM.
                   07  WRK-MAE-SANTEIYY        PIC 9(04).
                   07  WRK-MAE-SANTEIMM        PIC 9(02).
               05  WRK-MAE-SANTEIDD            PIC 9(02).
      *
      *    保険組合せ番号数字変換
           03  WRK-HKNCOMBINUM-X               PIC X(04).
           03  WRK-HKNCOMBINUM-9  REDEFINES  WRK-HKNCOMBINUM-X
                                               PIC 9(04).
      *    後期高齢者非該当フラグ
           03  WRK-KOUKI-FLG                   PIC 9(01).
      *
           03  WRK-TEKSTYMD.
               05  WRK-TEKSTYYMM               PIC 9(06).
               05  WRK-TEKSTDD                 PIC 9(02).
      *
      *    レセプト種別判定用領域
       01  WK-RECESYUBETU-AREA.
           03  WK-KOKUHO                       PIC X(01).
           03  WK-IHO                          PIC X(01).
           03  WK-RJN                          PIC X(01).
           03  WK-KOHI-CNT                     PIC 9(02).
           03  WK-HONKZKKBN                    PIC X(01).
           03  WRK-AGE                         PIC 9(03).
           03  WRK-RECESYUBETU                 PIC 9(04).
           03  WRK-NYUGAIKBN                   PIC X(01).
      *
       01  WRK-CONS-AREA.
      *    
      *    生活保護保険番号 
           03  WRK-CONS-SEIHO-HKNNUM   PIC X(03)   VALUE   "012". 
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".       
      *    原爆保険番号 
           03  WRK-CONS-GENBAKU-HKNNUM PIC X(03)   VALUE   "019".       
      *    公害保険番号 
           03  WRK-CONS-KOUGAI-HKNNUM PIC X(03)    VALUE   "975".       
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
           COPY    "CPSK5000.INC".
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
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
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    レセプト種別返却サブ
           COPY    "CPORCSSYUBETUGET.INC".
      *
      *    入院料未算定日取得サブ（同日再入院の保険取得に使用）
           COPY    "CPORCSNYUINDAY.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    日次統計用入院情報パラメタ
           COPY    "CPORCBT120SUB.INC".
      *
       PROCEDURE                   DIVISION    USING
            ORCBT120SUB-AREA
            SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-ORCBT120SUB-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-ORCBT120SUB-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-ORCBT120SUB-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    主処理
           IF      LNK-ORCBT120SUB-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
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
           IF    ( LNK-ORCBT120SUB-SANTEIYMD    =   SPACE )
               MOVE    1               TO  LNK-ORCBT120SUB-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    患者マスター検索
           INITIALIZE                      PTINF-REC
           MOVE    LNK-ORCBT120SUB-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-ORCBT120SUB-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    2               TO  LNK-ORCBT120SUB-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    診療科チェック
           IF     LNK-ORCBT120SUB-SRYKA    NOT =   SPACE
               CONTINUE
           ELSE
               MOVE    3               TO  LNK-ORCBT120SUB-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    入院中のチェック（入院会計マスター検索）
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-ORCBT120SUB-HOSPNUM TO  NACCT-HOSPNUM
           MOVE    LNK-ORCBT120SUB-PTID    TO  NACCT-PTID
           MOVE    LNK-ORCBT120SUB-SANTEIYMD(1:6)
                                           TO  NACCT-SRYYM
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           NOT =   ZERO
               MOVE    4               TO  LNK-ORCBT120SUB-RC
      *        入院会計クローズ
               MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
               MOVE    "key17"                 TO  MCP-PATHNAME
               PERFORM   910-DB-CLOSE-SEC
               GO  TO  100-PRM-CHK-EXT
           ELSE
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT          NOT =   ZERO
                    MOVE    4              TO  LNK-ORCBT120SUB-RC
      *            入院会計クローズ
                    MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                    MOVE    "key17"                 TO  MCP-PATHNAME
                    PERFORM   910-DB-CLOSE-SEC
                    GO  TO  100-PRM-CHK-EXT
               ELSE
                    MOVE       LNK-ORCBT120SUB-SANTEIYMD
                                           TO  WRK-SANTEIYMD
                    IF     NACCT-DAY(WRK-SANTEIDD)   =  ZERO
                        MOVE   4              TO  LNK-ORCBT120SUB-RC
                    END-IF
               END-IF
           END-IF
      *
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *入院科／入退院区分／病棟番号／病室番号／退院事由／ドクターの取得
           MOVE    ZERO                     TO  FLG-NYUIN-HIT1
           INITIALIZE                           PTNYUINRRK-REC
           MOVE    LNK-ORCBT120SUB-HOSPNUM  TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-ORCBT120SUB-PTID     TO  PTNYUINRRK-PTID
           MOVE    LNK-ORCBT120SUB-RRKNUM   TO  PTNYUINRRK-RRKNUM
           MOVE    1                        TO  PTNYUINRRK-RRKEDANUM
           MOVE    "tbl_ptnyuinrrk"         TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK            =   ZERO
               MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK    =   1)  OR
                                 (FLG-NYUIN-HIT1    =   1)
                   IF   PTNYUINRRK-JTIKBN    =    "5" OR "6"
                       CONTINUE
                   ELSE
                       IF   (PTNYUINRRK-TENNYUYMD <=
                               LNK-ORCBT120SUB-SANTEIYMD)   AND
                            (PTNYUINRRK-TENSTUYMD >=
                               LNK-ORCBT120SUB-SANTEIYMD)
                           MOVE    1      TO   FLG-NYUIN-HIT1
      *                    入院科
                           MOVE    PTNYUINRRK-NYUINKA
                                         TO  LNK-ORCBT120SUB-NYUINKA
      *                    入退院区分
                           MOVE   3      TO  LNK-ORCBT120SUB-NYUIN-KBN
                           IF    LNK-ORCBT120SUB-SANTEIYMD  =  
                                                   PTNYUINRRK-NYUINYMD
                               MOVE   1  TO  LNK-ORCBT120SUB-NYUIN-KBN
                           END-IF
                           IF    LNK-ORCBT120SUB-SANTEIYMD  =  
                                                   PTNYUINRRK-TAIINYMD
                               MOVE   2  TO  LNK-ORCBT120SUB-NYUIN-KBN
                           END-IF
      *                    病棟番号
                           MOVE   PTNYUINRRK-BTUNUM
                                         TO  LNK-ORCBT120SUB-BTUNUM
      *                    病室番号
                           MOVE   PTNYUINRRK-BRMNUM(3:6)
                                         TO  LNK-ORCBT120SUB-BRMNUM
      *                    退院事由
                           MOVE   PTNYUINRRK-TAIINCD
                                         TO  LNK-ORCBT120SUB-TAIINCD
      *                    医師コード
                           MOVE   PTNYUINRRK-DRCD1
                                         TO  LNK-ORCBT120SUB-DRCD
                       END-IF
                   END-IF
                   MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                   MOVE    "key"                TO  MCP-PATHNAME
                   PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
           ELSE
               MOVE     5      TO      LNK-ORCBT120SUB-RC
           END-IF
      *    患者入院履歴クローズ
           MOVE    "tbl_ptnyuinrrk"         TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *前日入院科／前日病棟番号／前日病室番号の取得
      *    前日日の算定
           INITIALIZE                           STS-AREA-DAY
           INITIALIZE                           LNK-DAY6-AREA
           MOVE    "61"                     TO  LNK-DAY6-IRAI
           MOVE    LNK-ORCBT120SUB-SANTEIYMD TO LNK-DAY6-KIJUN
           MOVE    "1"                      TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                       TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"                USING  STS-AREA-DAY
                                                   LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN          TO  WRK-MAE-SANTEIYMD
      *    入院履歴の検索
           INITIALIZE                           PTNYUINRRK-REC
           MOVE    LNK-ORCBT120SUB-HOSPNUM  TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-ORCBT120SUB-PTID     TO  PTNYUINRRK-PTID
           MOVE    "tbl_ptnyuinrrk"         TO  MCP-TABLE
           MOVE    "key3"                   TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK            =   ZERO
               MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
               MOVE    "key3"               TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK    =   1)
                   IF   PTNYUINRRK-JTIKBN    =    "5" OR "6"
                       CONTINUE
                   ELSE
                       IF   (PTNYUINRRK-TENNYUYMD <=
                               WRK-MAE-SANTEIYMD)   AND
                            (PTNYUINRRK-TENSTUYMD >=
                               WRK-MAE-SANTEIYMD)
      *                    入院科
                           MOVE    PTNYUINRRK-NYUINKA
                                       TO  LNK-ORCBT120SUB-MAE-NYUINKA
      *                    病棟番号
                           MOVE   PTNYUINRRK-BTUNUM
                                       TO  LNK-ORCBT120SUB-MAE-BTUNUM
      *                    病室番号
                           MOVE   PTNYUINRRK-BRMNUM(3:6)
                                       TO  LNK-ORCBT120SUB-MAE-BRMNUM
                       END-IF
                   END-IF
                   MOVE    "tbl_ptnyuinrrk"      TO  MCP-TABLE
                   MOVE    "key3"                TO  MCP-PATHNAME
                   PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
           ELSE
               MOVE     5      TO      LNK-ORCBT120SUB-RC
           END-IF
      *    患者入院履歴クローズ
           MOVE    "tbl_ptnyuinrrk"         TO  MCP-TABLE
           MOVE    "key3"                   TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    レセ電入院科の取得(算定日）
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    LNK-ORCBT120SUB-NYUINKA
                                       TO  SYS-1005-KBNCD
           MOVE    LNK-ORCBT120SUB-SANTEIYMD
                                       TO  SYS-1005-STYUKYMD
                                           SYS-1005-EDYUKYMD
           MOVE SPA-HOSPNUM            TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"     TO  MCP-TABLE
               MOVE    "key10"            TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-1005-REC
      *            レセ電診療科
                   IF      SYS-1005-RECESRYKA  =   SPACE
                       MOVE    SPACE       TO  
                                         LNK-ORCBT120SUB-RECENYUINKA
                   ELSE
                       MOVE    SYS-1005-RECESRYKA
                                     TO  LNK-ORCBT120SUB-RECENYUINKA
                   END-IF
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"           TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    レセ電入院科の取得(前算定日）
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    LNK-ORCBT120SUB-MAE-NYUINKA
                                       TO  SYS-1005-KBNCD
           MOVE    LNK-ORCBT120SUB-SANTEIYMD
                                       TO  SYS-1005-STYUKYMD
                                           SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-1005-REC
      *            レセ電診療科
                   IF      SYS-1005-RECESRYKA  =   SPACE
                       MOVE    SPACE       TO  
                                     LNK-ORCBT120SUB-MAE-RECENYUINKA
                   ELSE
                       MOVE    SYS-1005-RECESRYKA
                                 TO  LNK-ORCBT120SUB-MAE-RECENYUINKA
                   END-IF
               END-IF
           END-IF
      *   システム管理クローズ
           MOVE    "tbl_syskanri"           TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *入院時の外泊取得
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-ORCBT120SUB-HOSPNUM TO  NACCT-HOSPNUM
           MOVE    LNK-ORCBT120SUB-PTID    TO  NACCT-PTID
           MOVE    LNK-ORCBT120SUB-SANTEIYMD(1:6)
                                           TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               MOVE    NACCT-DAY(WRK-SANTEIDD)
                                 TO  LNK-ORCBT120SUB-GAIHAKU-KBN
           END-IF
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *食事情報の取得（入院会計）
           IF     LNK-ORCBT120SUB-GAIHAKU-KBN  =  1  OR  2  OR  3
               CONTINUE
           ELSE
               IF    LNK-ORCBT120SUB-SANTEIYMD(1:6)   >=  "200604"
                   PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
                     UNTIL   IDX-SKJ-TAIHI  >    3
                     INITIALIZE                          NYUINACCT-REC
                     MOVE    LNK-ORCBT120SUB-HOSPNUM TO  NACCT-HOSPNUM
                     MOVE    LNK-ORCBT120SUB-PTID    TO  NACCT-PTID
                     MOVE    LNK-ORCBT120SUB-SANTEIYMD(1:6)
                                                     TO  NACCT-SRYYM
                     MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                     EVALUATE   IDX-SKJ-TAIHI
                         WHEN    1
                             MOVE    "key30"     TO  MCP-PATHNAME
                         WHEN    2
                             MOVE    "key31"     TO  MCP-PATHNAME
                         WHEN    3
                             MOVE    "key32"     TO  MCP-PATHNAME
                     END-EVALUATE
                     PERFORM 910-NYUINACCT-SELECT-SEC
                     IF      FLG-NYUINACCT           =   ZERO
                         MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                         EVALUATE   IDX-SKJ-TAIHI
                             WHEN    1
                                 MOVE    "key30"     TO  MCP-PATHNAME
                             WHEN    2
                                 MOVE    "key31"     TO  MCP-PATHNAME
                             WHEN    3
                                 MOVE    "key32"     TO  MCP-PATHNAME
                         END-EVALUATE
                         PERFORM   910-NYUINACCT-READ-SEC
                         IF      NACCT-DAY(WRK-SANTEIDD)  >
                                              LNK-ORCBT120SUB-SKJ-KBN
                             MOVE   NACCT-DAY(WRK-SANTEIDD)   TO
                                              LNK-ORCBT120SUB-SKJ-KBN
                         END-IF
                     END-IF
      *             入院会計クローズ
                     MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                     EVALUATE   IDX-SKJ-TAIHI
                         WHEN    1
                             MOVE    "key30"     TO  MCP-PATHNAME
                         WHEN    2
                             MOVE    "key31"     TO  MCP-PATHNAME
                         WHEN    3
                             MOVE    "key32"     TO  MCP-PATHNAME
                     END-EVALUATE
                     PERFORM   910-DB-CLOSE-SEC
                   END-PERFORM
               ELSE
                   INITIALIZE                          NYUINACCT-REC
                   MOVE    LNK-ORCBT120SUB-HOSPNUM TO  NACCT-HOSPNUM
                   MOVE    LNK-ORCBT120SUB-PTID    TO  NACCT-PTID
                   MOVE    LNK-ORCBT120SUB-SANTEIYMD(1:6)
                                                   TO  NACCT-SRYYM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key28"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-SELECT-SEC
                   IF      FLG-NYUINACCT           =   ZERO
                       MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                       MOVE    "key28"             TO  MCP-PATHNAME
                       PERFORM   910-NYUINACCT-READ-SEC
                       MOVE    NACCT-DAY(WRK-SANTEIDD)
                                         TO  LNK-ORCBT120SUB-SKJ-KBN
                   END-IF
      *           入院会計クローズ
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key28"                 TO  MCP-PATHNAME
                   PERFORM   910-DB-CLOSE-SEC
               END-IF
           END-IF
      *
      *    収納情報の取得
           MOVE    ZERO                     TO  SYU-IDX
           INITIALIZE                           SYUNOU-REC
           MOVE    LNK-ORCBT120SUB-HOSPNUM  TO  SYU-HOSPNUM
           MOVE    "1"                      TO  SYU-NYUGAIKBN
           MOVE    LNK-ORCBT120SUB-PTID     TO  SYU-PTID
           MOVE    "tbl_syunou"             TO  MCP-TABLE
           MOVE    "key2"                   TO  MCP-PATHNAME
           PERFORM 910-SYUNOU-SELECT-SEC
           IF      FLG-SYUNOU                =  ZERO
               MOVE    "tbl_syunou"         TO  MCP-TABLE
               MOVE    "key2"               TO  MCP-PATHNAME
               PERFORM   910-SYUNOU-READ-SEC
               PERFORM   UNTIL   (FLG-SYUNOU    =   1)   OR
                                 (SYU-IDX       >=  10)
                   IF   SYU-DENPJTIKBN    =    "3"
                       CONTINUE
                   ELSE
                       IF   SYU-DENPPRTYMD = LNK-ORCBT120SUB-SANTEIYMD
                           PERFORM   210-SYUNOU-SET-SEC
                       END-IF
                   END-IF
                   MOVE    "tbl_syunou"       TO  MCP-TABLE
                   MOVE    "key2"             TO  MCP-PATHNAME
                   PERFORM 910-SYUNOU-READ-SEC
               END-PERFORM
           END-IF
      *    収納クローズ
           MOVE    "tbl_syunou"             TO  MCP-TABLE
           MOVE    "key2"                   TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    保険組合せ番号の取得（収納が無い場合に入院会計から取得）
           IF    LNK-ORCBT120SUB-HKNCOMBINUM(1)    =   ZERO
               INITIALIZE                          NYUINACCT-REC
               MOVE    LNK-ORCBT120SUB-HOSPNUM TO  NACCT-HOSPNUM
               MOVE    LNK-ORCBT120SUB-PTID    TO  NACCT-PTID
               MOVE    LNK-ORCBT120SUB-SANTEIYMD(1:6)
                                               TO  NACCT-SRYYM
               MOVE    "5"                     TO  NACCT-ZAISKBKBN
               MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
               MOVE    "key17"                 TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-SELECT-SEC
               IF      FLG-NYUINACCT           =   ZERO
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM   910-NYUINACCT-READ-SEC
                   MOVE    NACCT-DAY(WRK-SANTEIDD)
                               TO  LNK-ORCBT120SUB-HKNCOMBINUM(1)
               END-IF
      *        入院会計クローズ
               MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
               MOVE    "key17"                 TO  MCP-PATHNAME
               PERFORM   910-DB-CLOSE-SEC
      *
      *        同日再入院判定
               IF    LNK-ORCBT120SUB-NYUIN-KBN    =    1
                 INITIALIZE                        ORCSNYUINDAY-AREA
                 MOVE  LNK-ORCBT120SUB-HOSPNUM TO  LNK-NYUINDAY-HOSPNUM
                 MOVE  LNK-ORCBT120SUB-PTID    TO  LNK-NYUINDAY-PTID
                 MOVE  LNK-ORCBT120SUB-SANTEIYMD(1:6)
                                               TO  LNK-NYUINDAY-SANTEIYM
                 CALL  "ORCSNYUINDAY"   USING      ORCSNYUINDAY-AREA
                                                   SPA-AREA
                 IF    LNK-NYUINDAY-DOUJITSU-KBN(WRK-SANTEIDD)  =  "1"
                   MOVE  LNK-NYUINDAY-DOUJITSU-HKNCOMBI(WRK-SANTEIDD)
                                     TO  LNK-ORCBT120SUB-HKNCOMBINUM(1)
                 END-IF
               END-IF
           END-IF
      *
      *
      *    保険情報の取得
           PERFORM    VARYING   SYU-IDX   FROM  1  BY  1
             UNTIL   (LNK-ORCBT120SUB-HKNCOMBINUM(SYU-IDX) = ZERO) OR
                     (SYU-IDX     >     10)
               INITIALIZE                      HKNCOMBI-REC
               MOVE    LNK-ORCBT120SUB-HOSPNUM
                                        TO     COMB-HOSPNUM
               MOVE    LNK-ORCBT120SUB-PTID
                                        TO      COMB-PTID
               MOVE    LNK-ORCBT120SUB-HKNCOMBINUM(SYU-IDX)
                                        TO      COMB-HKNCOMBINUM
               MOVE    "tbl_hkncombi"   TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-SELECT-SEC
               IF    FLG-HKNCOMBI       =       ZERO
                   MOVE    "tbl_hkncombi"   TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-HKNCOMBI-READ-SEC
                   IF    FLG-HKNCOMBI   =       ZERO
                       MOVE   COMB-HKNNUM
                                  TO  LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                       MOVE   COMB-HKNID
                                  TO  LNK-ORCBT120SUB-HKNID(SYU-IDX)
                       MOVE   COMB-HONKZKKBN
                               TO  LNK-ORCBT120SUB-HONKZKKBN(SYU-IDX)
                       MOVE   COMB-HOJOKBN
                                  TO  LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                       MOVE   COMB-CONTKBN
                                  TO  LNK-ORCBT120SUB-CONTKBN(SYU-IDX)
                       PERFORM   220-HKNJANUM-SET-SEC
                       IF        COMB-HKNNUM  =  "971"  OR  "973"
                           PERFORM   230-PTRSIINF-SET-SEC
                       END-IF
                       PERFORM   VARYING  COMB-IDX  FROM  1  BY  1
                                 UNTIL   (COMB-KOHID(COMB-IDX) = ZERO)
                                    OR   (COMB-IDX    >   4)
                           MOVE   COMB-KOHHKNNUM(COMB-IDX)
                               TO  LNK-ORCBT120SUB-KOH-HKNNUM
                                                 (SYU-IDX COMB-IDX)
                           MOVE   COMB-KOHID(COMB-IDX)
                               TO  LNK-ORCBT120SUB-KOH-ID
                                                 (SYU-IDX COMB-IDX)
                           MOVE   COMB-KOHPAYKBN(COMB-IDX)
                               TO  LNK-ORCBT120SUB-KOH-PAYKBN
                                                 (SYU-IDX COMB-IDX)
                       END-PERFORM
                   END-IF
               END-IF
      *        保険組合せクローズ
               MOVE    "tbl_hkncombi"   TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               PERFORM   910-DB-CLOSE-SEC
           END-PERFORM
      *
      *    レセプト種別の取得
           PERFORM    VARYING   SYU-IDX   FROM  1  BY  1
             UNTIL   (LNK-ORCBT120SUB-HKNCOMBINUM(SYU-IDX) = ZERO) OR
                     (SYU-IDX     >     10)
               PERFORM   240-RECESYUBETU-SET-SEC
               MOVE      WRK-RECESYUBETU    TO
                                 LNK-ORCBT120SUB-RECESYUBETU(SYU-IDX)
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納セット処理
      *****************************************************************
       210-SYUNOU-SET-SEC         SECTION.
      *
           ADD    1     TO   SYU-IDX
      *    伝票番号
           MOVE    SYU-DENPNUM         TO  LNK-ORCBT120SUB-DENPNUM
                                                           (SYU-IDX)
      *    伝票状態区分
           MOVE    SYU-DENPJTIKBN      TO  LNK-ORCBT120SUB-DENPJTIKBN
                                                           (SYU-IDX)
      *    伝票作成区分
           MOVE    SYU-CREATEKBN       TO  LNK-ORCBT120SUB-CREATEKBN
                                                           (SYU-IDX)
      *    保険点数
           MOVE    SYU-TOTAL-HKNTEN    TO  LNK-ORCBT120SUB-HKNTEN
                                                           (SYU-IDX)
      *    保険負担金額
           COMPUTE    LNK-ORCBT120SUB-HKNMONEY (SYU-IDX)
                                       =   SYU-HKNTEKMONEY
                                       +   SYU-RJN-FTN
                                       +   SYU-KOH-FTN
      *    保険適用外金額
           COMPUTE    LNK-ORCBT120SUB-TGMONEY (SYU-IDX)
                                       =   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
                                       -   SYU-TGMONEY-TAX-SAI
      *    保険適用外税額
           MOVE    SYU-TGMONEY-TAX-SAI
                                       TO  LNK-ORCBT120SUB-TGMONEY-TAX
                                                           (SYU-IDX)
      *    自費金額 
           COMPUTE LNK-ORCBT120SUB-JIHI (SYU-IDX)
                                       =  SYU-JIHI-TOTAL
                                       +  SYU-JIHI-TOTAL-TAX
      *    自費税額
           MOVE    SYU-JIHI-TAX        TO  LNK-ORCBT120SUB-JIHI-TAX
                                                           (SYU-IDX)
      *    食事負担金額
           COMPUTE  LNK-ORCBT120SUB-SKJ (SYU-IDX)
                                       =   SYU-SKYMONEY-SKJ
                                       +   SYU-SKYMONEY-SKJ-JIHI
                                       +   SYU-SKYMONEY-LIFE
                                       +   SYU-SKYMONEY-LIFE-JIHI
      *    食事消費税額
           COMPUTE  LNK-ORCBT120SUB-SKJ-TAX (SYU-IDX)
                                       =   SYU-SKYMONEY-SKJ-TAX
                                       +   SYU-SKYMONEY-SKJ-JIHI-TAX
                                       +   SYU-SKYMONEY-LIFE-TAX
                                       +   SYU-SKYMONEY-LIFE-JIHI-TAX
      *    室料差額
           COMPUTE  LNK-ORCBT120SUB-RMSAGAKU (SYU-IDX)
                                       =   SYU-RMSAGAKU
                                       -   SYU-RMSAGAKU-TAX-SAI
      *    室料差額税額
           MOVE     SYU-RMSAGAKU-TAX-SAI   TO  
                                LNK-ORCBT120SUB-RMSAGAKU-TAX (SYU-IDX)
      *    請求金額
           MOVE    SYU-SKYMONEY        TO  LNK-ORCBT120SUB-SKYMONEY
                                                           (SYU-IDX)
      *    労災／自賠責がある場合の請求額
           IF      SYU-RSIJIBAI-SKYMONEY   NOT =  ZERO
               MOVE   SYU-RSIJIBAI-SKYMONEY
                                       TO  LNK-ORCBT120SUB-SKYMONEY
                                                           (SYU-IDX)
           END-IF
      *
      *    保険組合せ番号
           MOVE    SYU-HKNCOMBINUM     TO  WRK-HKNCOMBINUM-X
           MOVE    WRK-HKNCOMBINUM-9   TO  LNK-ORCBT120SUB-HKNCOMBINUM
                                                           (SYU-IDX)
      *
           .
       210-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号セット処理
      *****************************************************************
       220-HKNJANUM-SET-SEC         SECTION.
      *
           INITIALIZE                      PTHKNINF-REC
           MOVE    LNK-ORCBT120SUB-HOSPNUM
                                    TO     PTHKN-HOSPNUM
           MOVE    LNK-ORCBT120SUB-PTID
                                    TO     PTHKN-PTID
           MOVE    COMB-HKNID       TO     PTHKN-HKNID
           MOVE    "tbl_pthkninf"   TO  MCP-TABLE
           MOVE    "key"            TO  MCP-PATHNAME
           PERFORM 910-PTHKNINF-SELECT-SEC
           IF    FLG-PTHKNINF       =       ZERO
               MOVE    "tbl_pthkninf"   TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
               IF    FLG-PTHKNINF   =       ZERO
                   MOVE   PTHKN-HKNJANUM
                              TO  LNK-ORCBT120SUB-HKNJANUM(SYU-IDX)
               END-IF
           END-IF
      *    患者保険情報クローズ
           MOVE    "tbl_pthkninf"   TO  MCP-TABLE
           MOVE    "key"            TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       220-HKNJANUN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災情報セット処理
      *****************************************************************
       230-PTRSIINF-SET-SEC         SECTION.
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    LNK-ORCBT120SUB-HOSPNUM
                                    TO     PTRSI-HOSPNUM
           MOVE    LNK-ORCBT120SUB-PTID
                                    TO     PTRSI-PTID
           MOVE    COMB-HKNID       TO     PTRSI-HKNID
           MOVE    "tbl_ptrsiinf"   TO  MCP-TABLE
           MOVE    "key"            TO  MCP-PATHNAME
           PERFORM 910-PTRSIINF-SELECT-SEC
           IF    FLG-PTRSIINF       =       ZERO
               MOVE    "tbl_ptrsiinf"   TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               PERFORM 910-PTRSIINF-READ-SEC
               IF    FLG-PTRSIINF   =       ZERO
                   MOVE   PTRSI-HKNKBN
                              TO  LNK-ORCBT120SUB-RSIHKNKBN(SYU-IDX)
                   MOVE   PTRSI-JIBAISEIKBN
                              TO  LNK-ORCBT120SUB-JIBAIKBN(SYU-IDX)
                   MOVE   PTRSI-SAIGAIKBN
                              TO  LNK-ORCBT120SUB-SAIGAIKBN(SYU-IDX)
               END-IF
           END-IF
      *    患者労災保険情報クローズ
           MOVE    "tbl_ptrsiinf"   TO  MCP-TABLE
           MOVE    "key"            TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       230-PTRSIINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別セット処理
      *****************************************************************
       240-RECESYUBETU-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WK-RECESYUBETU-AREA
           INITIALIZE                      WK-RECESYUBETU-AREA
           MOVE    "1"                 TO  WRK-NYUGAIKBN
      *
      *    労災・自賠責のとき
           IF      LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                                       =   WRK-CONS-ROUSAI-HKNNUM  OR
                                           WRK-CONS-JIBAI-HKNNUM
               EVALUATE    LNK-ORCBT120SUB-RSIHKNKBN(SYU-IDX)
                                       ALSO
                           LNK-ORCBT120SUB-SAIGAIKBN(SYU-IDX)
                                       ALSO
                           WRK-NYUGAIKBN
                   WHEN    "1"         ALSO    "1"     ALSO    "1"   
                       MOVE    7081        TO    WRK-RECESYUBETU
                   WHEN    "1"         ALSO    "2"     ALSO    "1"
                       MOVE    7082        TO    WRK-RECESYUBETU
                   WHEN    "2"         ALSO    "1"     ALSO    "1"
                       MOVE    7101        TO    WRK-RECESYUBETU
                   WHEN    "2"         ALSO    "2"     ALSO    "1"
                       MOVE    7102        TO    WRK-RECESYUBETU
                   WHEN    "1"         ALSO    "1"     ALSO    "2"   
                       MOVE    7071        TO    WRK-RECESYUBETU
                   WHEN    "1"         ALSO    "2"     ALSO    "2"
                       MOVE    7072        TO    WRK-RECESYUBETU
                   WHEN    "2"         ALSO    "1"     ALSO    "2"
                       MOVE    7091        TO    WRK-RECESYUBETU
                   WHEN    "2"         ALSO    "2"     ALSO    "2"
                       MOVE    7092        TO    WRK-RECESYUBETU
                   WHEN    "3"         ALSO    ANY     ALSO    ANY
                       MOVE    7021        TO    WRK-RECESYUBETU
                   WHEN    "4"         ALSO    ANY     ALSO    ANY
                       MOVE    7011        TO    WRK-RECESYUBETU
      *            公務災害
                   WHEN    "5"         ALSO    ANY     ALSO    "1"
                       MOVE    7061        TO    WRK-RECESYUBETU
                   WHEN    "5"         ALSO    ANY     ALSO    "2"
                       MOVE    7051        TO    WRK-RECESYUBETU
                   END-EVALUATE
               GO  TO  240-RECESYUBETU-SET-EXT
           END-IF
      *
      *    自費のとき
           IF     LNK-ORCBT120SUB-HKNNUM(SYU-IDX)(1:2) =   "98"
               MOVE    8888                TO  WRK-RECESYUBETU
               GO  TO  240-RECESYUBETU-SET-EXT
           END-IF
      *
      *    治験のとき
           IF     LNK-ORCBT120SUB-SANTEIYMD    >=  "20080401"
               IF    (LNK-ORCBT120SUB-HKNNUM(SYU-IDX)  >=  "900")  AND
                     (LNK-ORCBT120SUB-HKNNUM(SYU-IDX)  <=  "919")
                   MOVE    8888                TO  WRK-RECESYUBETU
                   GO  TO  240-RECESYUBETU-SET-EXT
               END-IF
           END-IF
      *
      *    公害保険（入院）のとき
           IF     LNK-ORCBT120SUB-HKNNUM(SYU-IDX)  =
                                               WRK-CONS-KOUGAI-HKNNUM
               MOVE    7210                TO  WRK-RECESYUBETU
               GO  TO  240-RECESYUBETU-SET-EXT
           END-IF
      *
      *    年齢計算
           INITIALIZE                      PTINF-REC
           MOVE    LNK-ORCBT120SUB-HOSPNUM
                                       TO  PTINF-HOSPNUM
           MOVE    LNK-ORCBT120SUB-PTID
                                       TO  PTINF-PTID
           PERFORM    900-PTINF-READ-SEC
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    LNK-ORCBT120SUB-HOSPNUM
                                       TO  AGECHK-HOSPNUM
           MOVE    LNK-ORCBT120SUB-PTID
                                       TO  AGECHK-PTID
           MOVE    LNK-ORCBT120SUB-SANTEIYMD
                                       TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF      AGECHK-RC           =   ZERO
               MOVE    AGECHK-NENREI       TO  WRK-AGE
           ELSE
               MOVE    ZERO                TO  WRK-AGE
           END-IF
      *
           IF      LNK-ORCBT120SUB-HKNNUM(SYU-IDX)  NOT =   SPACE
               EVALUATE    LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                   WHEN    "060"
                   WHEN    "067"
                   WHEN    "068"
                   WHEN    "069"
                       IF    LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                                                   = "060" OR "068"
                           MOVE   "1"      TO  WK-KOKUHO
                       ELSE
                           MOVE   "2"      TO  WK-KOKUHO
                       END-IF
                   WHEN    "039"
                   WHEN    "040"
                       MOVE   "3"          TO  WK-KOKUHO
                   WHEN    OTHER
                       MOVE    "1"         TO  WK-IHO
               END-EVALUATE
           END-IF
      *
           IF      LNK-ORCBT120SUB-SANTEIYMD(1:6)    <      "200210"
               IF    LNK-ORCBT120SUB-KOH1HKNNUM(SYU-IDX)  =   "027"
                   MOVE        "1"             TO  WK-RJN
               END-IF
           END-IF
      *
           MOVE    LNK-ORCBT120SUB-HONKZKKBN(SYU-IDX) TO WK-HONKZKKBN
      *
           MOVE    ZERO                TO  WK-KOHI-CNT
           PERFORM VARYING IDX FROM    1   BY  1
             UNTIL  (IDX     >       4)   OR
                    (LNK-ORCBT120SUB-KOH-HKNNUM(SYU-IDX IDX) = SPACE)
               IF LNK-ORCBT120SUB-KOH-HKNNUM(SYU-IDX IDX) NOT = "027"
                   ADD     1               TO  WK-KOHI-CNT
               END-IF
           END-PERFORM
      *
           IF  LNK-ORCBT120SUB-HKNNUM(SYU-IDX)  NOT =   SPACE
      *
               IF    ( LNK-ORCBT120SUB-SANTEIYMD(1:6)  >=  "200210"  )
               AND   ( LNK-ORCBT120SUB-SANTEIYMD(1:6)  <=  "200803"  )
      *            ３歳未満の場合、ＷＫ−ＨＯＮＫＺＫＫＢＮに"３"をセット
                   IF      WRK-AGE         <     3
                               MOVE   "3"     TO   WK-HONKZKKBN
                   END-IF
               END-IF
      *        平成２０年４月改正対応
               IF      LNK-ORCBT120SUB-SANTEIYMD(1:6)  >=  "200804"
                   IF      AGECHK-SYUGAKUKBN     =     1
                               MOVE   "3"     TO   WK-HONKZKKBN
                   END-IF
               END-IF
      *
      *        後期高齢者非該当
               MOVE    ZERO            TO  WRK-KOUKI-FLG
      *
               IF    ( LNK-ORCBT120SUB-SANTEIYMD(1:6)
                                      >=  "200804" )
               AND   ( WRK-AGE        >=  70       )
                   IF    ( LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                                       =   SPACE   OR  "040"   OR  "039"
                                       OR  "067"   OR  "069"           )
                   OR    ( LNK-ORCBT120SUB-HKNNUM(SYU-IDX)(1:2)
                                       =   "98"    OR  "90"    OR  "91")
                       CONTINUE
                   ELSE
                       IF   (  LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                                               =   "060"   OR  "068" )
                       OR   (( LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                                           NOT =   "060"   AND "068")
                               AND
                             ( LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                                               =   SPACE            ))
                           MOVE      LNK-ORCBT120SUB-SANTEIYMD(1:6)
                                                      TO  WRK-TEKSTYYMM
                           IF      COMB-TEKSTYMD(1:6)  >=
                                       LNK-ORCBT120SUB-SANTEIYMD(1:6)
                               MOVE    COMB-TEKSTYMD(7:2)
                                                       TO  WRK-TEKSTDD
                           ELSE
                                MOVE  01               TO  WRK-TEKSTDD
                           END-IF
      *
                           INITIALIZE                      PTKOHINF-REC
                           MOVE    LNK-ORCBT120SUB-HOSPNUM
                                                       TO  PTKOH-HOSPNUM
                           MOVE    COMB-PTID           TO  PTKOH-PTID
                           MOVE    "976"               TO  PTKOH-KOHNUM
                           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
                           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                           MOVE    "key4"              TO  MCP-PATHNAME
                           PERFORM 900-DBSELECT-SEC
                           IF      MCP-RC              =   ZERO
                               MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                               MOVE    "key4"          TO  MCP-PATHNAME
                               PERFORM 900-PTKOHINF-READ-N-SEC
                               PERFORM UNTIL   FLG-PTKOHINF    =   1
                                   IF    (PTKOH-TEKSTYMD
                                                       <=  WRK-TEKSTYMD)
                                   AND   (WRK-TEKSTYMD <=
                                                       PTKOH-TEKEDYMD  )
                                       MOVE    1       TO  WRK-KOUKI-FLG
                                       MOVE    1       TO  FLG-PTKOHINF
                                   ELSE
                                       MOVE    "tbl_ptkohinf"
                                                        TO  MCP-TABLE
                                       MOVE    "key4"   TO  MCP-PATHNAME
                                       PERFORM  900-PTKOHINF-READ-N-SEC
                                   END-IF
                               END-PERFORM
                           END-IF
                           MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                           MOVE    "key4"               TO  MCP-PATHNAME
                           PERFORM  910-DB-CLOSE-SEC
                       END-IF
                   END-IF
               END-IF
      *
      *            １割、２割負担の高齢者の場合ＷＫ−ＨＯＮＫＺＫＫＢＮに
      *            "９"または"８"をセット
      *            老人保険の場合はＷＫ−ＲＪＮにセットする
      *
      *        平成２５年５月以降
               IF     LNK-ORCBT120SUB-SANTEIYMD(1:6)  >=  "201405"
                   IF    ( LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                                           =   "002" )
                   AND  ( WRK-AGE          >=  75    )
                       EVALUATE    LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                           WHEN    "A"
                           WHEN    "G"
                           WHEN    "D"
                               MOVE    "1"
                                   TO  LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                           WHEN    "B"
                           WHEN    "H"
                           WHEN    "E"
                               MOVE    "2"
                                   TO  LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                           WHEN    "C"
                           WHEN    "I"
                           WHEN    "F"
                               MOVE    "3"
                                   TO  LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                       END-EVALUATE
                   END-IF
      *
                   EVALUATE    LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                       WHEN    "002" 
                           EVALUATE   LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
      *********************IF  (WRK-AGE        >=   70)  AND
      *********************    (WRK-AGE        <=   74)
                           IF  (WRK-AGE        >=   70)
                           AND (WRK-KOUKI-FLG  =    0 )
                              EVALUATE LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                                   WHEN   "1"
                                   WHEN   "5"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "6"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                                   WHEN   "3"
                                       MOVE   "7"   TO   WK-HONKZKKBN
                              END-EVALUATE
                           END-IF     
                       WHEN    "039" 
                       WHEN    "040" 
                           EVALUATE  LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "1"
                                   MOVE   "9"   TO   WK-RJN
                               WHEN   "3"
                                   MOVE   "7"   TO   WK-RJN
                           END-EVALUATE
                       WHEN    "067" 
                       WHEN    "069" 
                           IF      WRK-AGE  >=   65
                               MOVE   "?"   TO   WK-HONKZKKBN
                           ELSE
                              EVALUATE LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                                  WHEN   "9"
                                      MOVE   "9"   TO   WK-HONKZKKBN
                                  WHEN   "8"
                                      MOVE   "?"   TO   WK-HONKZKKBN
                                  WHEN   "7"
                                      MOVE   "7"   TO   WK-HONKZKKBN
                              END-EVALUATE
                           END-IF
                       WHEN    OTHER 
                           EVALUATE   LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
               END-IF
      *
      *        平成２０年４月〜平成２５年４月
               IF    ( LNK-ORCBT120SUB-SANTEIYMD(1:6)  >=  "200804" )
               AND   ( LNK-ORCBT120SUB-SANTEIYMD(1:6)  <=  "201404" )
                   IF    ( LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                                           =   "002" )
                   AND  ( WRK-AGE          >=  75    )
                       EVALUATE    LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                           WHEN    "A"
                           WHEN    "G"
                               MOVE    "1"
                                   TO  LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                           WHEN    "B"
                           WHEN    "H"
                               MOVE    "2"
                                   TO  LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                           WHEN    "C"
                           WHEN    "I"
                               MOVE    "3"
                                   TO  LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                       END-EVALUATE
                   END-IF
      *
                   EVALUATE    LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                       WHEN    "002" 
                           EVALUATE   LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
      *********************IF  (WRK-AGE        >=   70)  AND
      *********************    (WRK-AGE        <=   74)
                           IF  (WRK-AGE        >=   70)
                           AND (WRK-KOUKI-FLG  =    0 )
                              EVALUATE LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                                   WHEN   "1"
                                   WHEN   "5"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "6"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                                   WHEN   "3"
                                       MOVE   "7"   TO   WK-HONKZKKBN
                              END-EVALUATE
                           END-IF     
                       WHEN    "039" 
                       WHEN    "040" 
                           EVALUATE  LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "1"
                                   MOVE   "9"   TO   WK-RJN
                               WHEN   "3"
                                   MOVE   "7"   TO   WK-RJN
                           END-EVALUATE
                       WHEN    "067" 
                       WHEN    "069" 
                           IF      WRK-AGE  >=   65
                               MOVE   "?"   TO   WK-HONKZKKBN
                           ELSE
                              EVALUATE LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                                  WHEN   "9"
                                      MOVE   "9"   TO   WK-HONKZKKBN
                                  WHEN   "8"
                                      MOVE   "?"   TO   WK-HONKZKKBN
                                  WHEN   "7"
                                      MOVE   "7"   TO   WK-HONKZKKBN
                              END-EVALUATE
                           END-IF
                       WHEN    OTHER 
                           EVALUATE   LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
               END-IF
      *
      *        平成１８年１０月〜平成２０年３月
               IF    ( LNK-ORCBT120SUB-SANTEIYMD(1:6)  >=  "200610" )
               AND   ( LNK-ORCBT120SUB-SANTEIYMD(1:6)  <=  "200803" )
                   EVALUATE    LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                       WHEN    "002" 
                           EVALUATE   LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           IF  (WRK-AGE        >=   70)  AND
                               (WRK-AGE        <=   74)
                              EVALUATE LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                                   WHEN   "1"
                                   WHEN   "5"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "6"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                                   WHEN   "3"
                                       MOVE   "7"   TO   WK-HONKZKKBN
                              END-EVALUATE
                           END-IF     
                       WHEN    OTHER 
                           EVALUATE   LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
                   IF   LNK-ORCBT120SUB-KOH1HKNNUM(SYU-IDX)  =   "027"
                       EVALUATE   LNK-ORCBT120SUB-KOH1PAYKBN(SYU-IDX)
                           WHEN   "01"
                               MOVE   "9"   TO   WK-RJN
                           WHEN   "02"
                               MOVE   "8"   TO   WK-RJN
                           WHEN   "03"
                               MOVE   "7"   TO   WK-RJN
                       END-EVALUATE
                   END-IF
               END-IF
      *
      *        平成１４年１０月〜平成１８年９月
               IF    ( LNK-ORCBT120SUB-SANTEIYMD(1:6)  >=  "200210" )
               AND   ( LNK-ORCBT120SUB-SANTEIYMD(1:6)  <=  "200609" )
                   EVALUATE    LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                       WHEN    "002" 
                           EVALUATE   LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           IF  (WRK-AGE        >=   70)  AND
                               (WRK-AGE        <=   74)
                              EVALUATE LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                                   WHEN   "1"
                                   WHEN   "5"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "6"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                              END-EVALUATE
                           END-IF     
                       WHEN    OTHER 
                           EVALUATE   LNK-ORCBT120SUB-HOJOKBN(SYU-IDX)
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
                   IF   LNK-ORCBT120SUB-KOH1HKNNUM(SYU-IDX)  =   "027"
                       EVALUATE   LNK-ORCBT120SUB-KOH1PAYKBN(SYU-IDX)
                           WHEN   "01"
                               MOVE   "9"   TO   WK-RJN
                           WHEN   "02"
                               MOVE   "8"   TO   WK-RJN
                       END-EVALUATE
                   END-IF
               END-IF
      *
           END-IF
      *
           IF    ( WK-RJN          NOT =   SPACE )     
           AND   ( LNK-ORCBT120SUB-HONKZKKBN(SYU-IDX)  =   "1"   )     
               IF  LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                                       =   "063"   OR  "072"   OR
                                           "073"   OR  "074"   OR
                                           "075"
                   MOVE    "2"          TO  WK-IHO
               END-IF
           END-IF    
      *
      *      DISPLAY "HONKZKKBN : " WK-HONKZKKBN 
      *      DISPLAY "NYUGAIKBN : " TOUKEI01-NYUGAIKBN 
      *      DISPLAY "IHO       : " WK-IHO 
      *      DISPLAY "KOKUHO    : " WK-KOKUHO 
      *      DISPLAY "RJN       : " WK-RJN 
      *      DISPLAY "KOHI-CNT  : " WK-KOHI-CNT 
      *
           INITIALIZE                  ORCSSYUBETUGETAREA  
           MOVE    WK-KOKUHO           TO  LNK-SYUBETUGET-KOKUHO
           MOVE    WK-IHO              TO  LNK-SYUBETUGET-IHO
           MOVE    WK-RJN              TO  LNK-SYUBETUGET-RJN
           MOVE    WK-KOHI-CNT         TO  LNK-SYUBETUGET-KOHI-CNT
           MOVE    WK-HONKZKKBN        TO  LNK-SYUBETUGET-HONKZKKBN
           MOVE    "1"                 TO  LNK-SYUBETUGET-NYUGAIKBN
           MOVE    LNK-ORCBT120SUB-SANTEIYMD(1:6)
                                       TO  LNK-SYUBETUGET-SRYYM
           MOVE    LNK-ORCBT120SUB-HKNNUM(SYU-IDX)
                                       TO  LNK-SYUBETUGET-HKNNUM
           MOVE    PTINF-BIRTHDAY      TO  LNK-SYUBETUGET-BIRTHDAY 
      *
      *    国保のとき９７７の公費の有無
           IF    ( LNK-SYUBETUGET-BIRTHDAY
                                       <   19440402 )
           AND   ( LNK-SYUBETUGET-HKNNUM
                                       =   "060"    )
           AND   ( LNK-SYUBETUGET-SRYYM
                                       >   "201404" )
               MOVE    "99"                TO  LNK-SYUBETUGET-RJNPAYKBN
      *
               MOVE      LNK-ORCBT120SUB-SANTEIYMD(1:6)
                                           TO  WRK-TEKSTYYMM
               IF      COMB-TEKSTYMD(1:6)  >=
                                       LNK-ORCBT120SUB-SANTEIYMD(1:6)
                   MOVE    COMB-TEKSTYMD (7:2) TO  WRK-TEKSTDD
               ELSE
                   MOVE    01                  TO  WRK-TEKSTDD
               END-IF
      *
               INITIALIZE                      PTKOHINF-REC
               MOVE    LNK-ORCBT120SUB-HOSPNUM
                                           TO  PTKOH-HOSPNUM
               MOVE    COMB-PTID           TO  PTKOH-PTID
               MOVE    "977"               TO  PTKOH-KOHNUM
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 900-PTKOHINF-READ-N-SEC
                   PERFORM UNTIL   FLG-PTKOHINF    =   1
                       IF    ( PTKOH-TEKSTYMD  <=  WRK-TEKSTYMD   )
                       AND   ( WRK-TEKSTYMD    <=  PTKOH-TEKEDYMD )
                           MOVE    SPACE           TO
                                               LNK-SYUBETUGET-RJNPAYKBN
                           MOVE    1               TO  FLG-PTKOHINF
                       ELSE
                           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                           MOVE    "key4"          TO  MCP-PATHNAME
                           PERFORM  900-PTKOHINF-READ-N-SEC
                       END-IF
                   END-PERFORM
               END-IF
               MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM  910-DB-CLOSE-SEC
           END-IF
      *
           CALL    "ORCSSYUBETUGET"    USING   ORCSSYUBETUGETAREA
      *
           MOVE    LNK-SYUBETUGET-RECESYUBETU  TO    WRK-RECESYUBETU
           .
       240-RECESYUBETU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5000-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-5000-REC
               MOVE    1                   TO  FLG-SYSKANRI
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
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                 PTINF-REC
               MOVE    1              TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
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
       910-PTNYUINRRK-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTNYUINRRK-REC
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                     PTNYUINRRK-REC
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター検索
      *****************************************************************
       910-SYUNOU-SELECT-SEC        SECTION.
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SYUNOU
           ELSE
               MOVE    1                  TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       910-SYUNOU-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SYUNOU-REC
               MOVE    ZERO               TO  FLG-SYUNOU
           ELSE
               INITIALIZE                     SYUNOU-REC
               MOVE    1                  TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスター検索
      *****************************************************************
       910-PTHKNINF-SELECT-SEC        SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTHKNINF
           ELSE
               MOVE    1                  TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスター読込
      *****************************************************************
       910-PTHKNINF-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTHKNINF-REC
               MOVE    ZERO               TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                     PTHKNINF-REC
               MOVE    1                  TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報マスター検索
      *****************************************************************
       910-PTRSIINF-SELECT-SEC        SECTION.
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTRSIINF
           ELSE
               MOVE    1                  TO  FLG-PTRSIINF
           END-IF
      *
           .
       910-PTRSIINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報マスター読込
      *****************************************************************
       910-PTRSIINF-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTRSIINF-REC
               MOVE    ZERO               TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                     PTRSIINF-REC
               MOVE    1                  TO  FLG-PTRSIINF
           END-IF
      *
           .
       910-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       900-PTKOHINF-READ-N-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               IF      PTKOH-SAKJOKBN      =   "1"
                   GO  TO  900-PTKOHINF-READ-N-SEC
               END-IF
               MOVE    ZERO                TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1                   TO  FLG-PTKOHINF
           END-IF
      *
           .
       900-PTKOHINF-READ-N-EXT.
           EXIT.
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
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           PERFORM    900-ORCDBMAIN-SEC
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
