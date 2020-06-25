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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCSODR02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : オーダー編集（退院）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/11/11    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/05/28  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-ORDER               PIC 9(01).
           03  FLG-ODRITEM             PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-SYUMEI              PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-END                 PIC 9(01).
           03  FLG-ORCSNUM             PIC 9(01).
           03  FLG-ENTRYCHK            PIC 9(01).
           03  FLG-ITEM-CHK-G.
               05  FLG-ITEM-CHK        PIC 9(01)   OCCURS  20.
           03  FLG-TEIKIARI            PIC 9(01).
           03  FLG-SYUARI              PIC 9(01).
           03  FLG-ACCTARI             PIC 9(01).
           03  FLG-TEIKIJOB            PIC 9(01).
           03  FLG-ORCSDAY             PIC 9(01).
           03  FLG-PTTEIKIRRK          PIC 9(01).
           03  FLG-SYUDAY              PIC 9(01).
           03  FLG-HIT                 PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-NSRYSRH             PIC 9(01).
           03  FLG-BUNBEN              PIC 9(01).
           03  FLG-BUNBEN-NYUIN        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-TAIINCD             PIC 9(05).
           03  CNT-NYUINKA             PIC 9(05).
           03  CNT-HKNCOMBI            PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX3                    PIC 9(05).
           03  IDX4                    PIC 9(05).
           03  IDX-ODR                 PIC 9(05).
           03  IDX-SYU                 PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-KANACHK-MAE-INPUT   PIC X(500).
           03  WRK-SYS-KANRICD         PIC X(04).
           03  WRK-SYS-KBNCD           PIC X(08).
           03  WRK-SAGAKU              PIC 9(08).
           03  WRK-SNUM-INX            PIC X(26).
           03  WRK-SNUM-OUTNUM         PIC 9(12).
           03  WRK-RRKNUM              PIC 9(03).
           03  WRK-SHONUM              PIC 9(03).
           03  WRK-SKYSTYMD            PIC X(08).
           03  WRK-KIJUNYMD            PIC X(08).
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-KEISANYMD           PIC X(08).
           03  WRK-NYUINYMD            PIC X(08).
           03  WRK-TAIINYMD.
               05  WRK-TAIINYM         PIC X(06).
               05  WRK-TAIINDD         PIC 9(02).
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
           03  WRK-NISSU1              PIC 9(05).
           03  WRK-NISSU2              PIC 9(05).
           03  WRK-SYMD                PIC X(08).
           03  WRK-DENPNUM             PIC 9(07).
           03  WRK-DENPNUM-X           PIC X(08).
           03  WRK-NYUINKA             PIC X(02).
           03  WRK-BTUNUM              PIC X(02).
           03  WRK-BRMNUM              PIC X(08).
           03  WRK-DRCD                PIC X(04).
           03  WRK-DENPEDANUM          PIC 9(03).
           03  WRK-SKYMONEY            PIC  S9(07).
           03  WRK-TAIINCD             PIC X(02).
           03  WRK-DIALOG                 PIC 9(02).
           03  WRK-COMBI-HKNCOMBI      PIC 9(04).
           03  WRK-MCP-WINDOW          PIC X(64).
      *
       01  ITEM-AREA.
           03  ITEM-FORCE-UPDATE       PIC X(5).
           03  ITEM-TAIINCD            PIC X(02).
           03  ITEM-NYUINKA            PIC X(02).
           03  ITEM-HKNCOMBI           PIC 9(04).
      *
       01  ERRCD-AREA.
           03  ERRCD-ITEM-G.
               05  ERRCD-ITEM          PIC X(04).
               05  ERRCD-ORDER         PIC X(04).
           03  ERRCD-ERRCD             PIC X(04).
      *
       01  CONST-AREA.
           03  CONST-H180401           PIC X(08)   VALUE "20060401".
           03  CONST-H211001           PIC X(08)   VALUE "20091001".
           03  CONST-SYU-MAX           PIC 9(05)   VALUE   10.
           03  CONST-JOB-SHELLID       PIC X(08)   VALUE   "TEIKI1".
           03  CONST-JOB-JOBID         PIC 9(07)   VALUE   1.
           03  CONST-API               PIC 9(01)   VALUE   2.
           03  CONST-TRUE              PIC X(05)   VALUE   "TRUE".
           03  CONST-FALSE             PIC X(05)   VALUE   "FALSE".
      *
      *
      *    入院CLAIM項目IDコピー句
           COPY    "CPNCLAIMID.INC".
      *
      *    ORDERIDコピー句
           COPY    "CPNORDERID.INC".
      *
      *    患者入院履歴ワーク
       01  WKPTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //WKPTNYUINRRK-//.
      *
      *    患者入院履歴退避
       01  TRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //TRRK-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
           COPY    "CPSK0042.INC".
           COPY    "CPSK5013.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    入院診療行為照会
       01  NSRYSRH-REC.
           COPY    "CPNSRYSRH.INC".
      *
      *    オーダーテーブル
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    オーダ項目
       01  ODRITEM-REC.
           COPY    "CPODRITEM.INC".
      *
      *    患者マスタ
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
      *    収納明細マスター
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    患者定期請求履歴
        01 PTTEIKIRRK-REC.
           COPY     "CPPTTEIKIRRK.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    オーダー共通サブ
           COPY    "CPORCSODRS01.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    カナチェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    日報キー取得サブ
           COPY "CPORCSDAILYKEY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    病室名称編集
           COPY    "CPORCSBRMNUM.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    負担金計算サブ
       01  LNK-ORCS02-REC.
           COPY  "CPORCS02.INC".
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUDAY-REC-T.
         02 LNK-SYUDAY-REC     OCCURS 10.
           COPY  "CPSYUDAY.INC"  REPLACING  //SDAY-// BY //LNK-SDAY-//.
      *
      *    入院会計作成サブ
           COPY    "CPORCSODRNACCT.INC".
       01  LNK-PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //LNK-PTNYUINRRK-//.
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    定期請求ジョブチェック
           COPY    "CPORCSTEIKIJOB.INC".
      *
      *    収納更新差額取得
           COPY  "CPORCSS002.INC".
      *
      *    入院請求期間取得サブ
           COPY    "CPORCSS006.INC".
      *
      *    分娩情報編集サブ
           COPY    "CPORCSI02.INC".
       01  SNRRK-AREA.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //SNRRK-//.
      *
      *    入退院登録PUSH通知
           COPY    "CPORCSI03.INC".
      *
      *
      *    排他制御
           COPY    "CPORCSLOCK.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *    ユーザプログラム起動サブパラメタ
           COPY    "CPORCSUSERCHK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
      *    入院異動チェックサブ
           COPY    "CPORCSODR.INC".
       01  LNK-ODRITEM-REC.
           02  LNK-ODRITEM-MAX         PIC 9(03).
           02  LNK-ODRITEM-OCC         OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //LNK-ODRITEM-//.
      *
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING   SODR-AREA
                                               LNK-ODRITEM-REC
                                               MCPAREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  ERRCD-AREA
           INITIALIZE                  ITEM-AREA
      *
           INITIALIZE                  TRRK-REC
           INITIALIZE                  SODR-OT
      *
           IF    ( SODR-SRCKBN     =   "1" OR "2" )
               PERFORM 990-LOCK-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ITEM-G    =   SPACE )
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    SODR-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
               MOVE    SODR-PTID       TO  PTNYUINRRK-PTID
               PERFORM 900-PTNYUINRRK-KEY3-SEL-SEC
               IF    ( FLG-PTNYUINRRK  =   ZERO )
                   MOVE    PTNYUINRRK-REC
                                       TO  TRRK-REC
               ELSE
                   MOVE    "0051"      TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( SODR-KEYGENERATE    =   2 )
               MOVE    PTNYUINRRK-NYUINKA     TO  SODR-SRYKA
                                                  ITEM-NYUINKA
               MOVE    PTNYUINRRK-HKNCOMBINUM TO  SODR-HKNCOMBI
                                                  ITEM-HKNCOMBI
           END-IF
      *
           IF    ( ERRCD-ITEM-G    =   SPACE )
               PERFORM 900-BUNBEN-KEY3-SEL-SEC
               IF    ( FLG-BUNBEN  =   ZERO )
                 AND ( BUNBEN-BUNBENKBN    =   "1" OR "2" )
                 AND ( SODR-SRYYMD        >=   CONST-H211001 )
                   MOVE    1           TO  FLG-BUNBEN-NYUIN
               END-IF
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( ERRCD-ORDER  =   SPACE )
            AND  ( ERRCD-ITEM   =   SPACE )
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER  =   SPACE )
            AND  ( ERRCD-ITEM   =   SPACE )
               PERFORM 2002-KANREN-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
            AND  ( ERRCD-ITEM      =   SPACE )
               PERFORM 2101-ORCSNYUFTN-CALL-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
            AND  ( ERRCD-ITEM      =   SPACE )
               PERFORM VARYING IDX-SYU     FROM    1   BY  1
                       UNTIL ( IDX-SYU         >   CONST-SYU-MAX )
                        OR   ( LNK-SYU-SKYSTYMD(IDX-SYU)
                                               =   SPACE )
                        OR   ( ERRCD-ORDER NOT =   SPACE )
      *
                   PERFORM 2101-SYUNOU-HEN-SEC
      *
               END-PERFORM
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
            AND  ( ERRCD-ITEM      =   SPACE )
      *
               IF    ( FLG-END =   ZERO )
                   PERFORM 2101-PTNYUINRRK-UPDATE-SEC
               END-IF
      *
               IF    ( FLG-END =   ZERO )
                   IF    ( FLG-SYUARI      =   1 )
                       PERFORM 2101-TORIKESI-KOUSHIN-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-END =   ZERO )
                   PERFORM 2101-ORCSODRNACCT-CALL-SEC
               END-IF
      *
               PERFORM VARYING IDX-SYU     FROM    1   BY  1
                       UNTIL ( IDX-SYU         >   CONST-SYU-MAX )
                        OR   ( LNK-SYU-SKYSTYMD(IDX-SYU)
                                               =   SPACE )
                        OR   ( FLG-END         =   1 )
      *
      *            収納マスタ更新処理
                   PERFORM 2102-SYUNOU-UPD-SEC
      *
               END-PERFORM
      *
               IF    ( FLG-END  =   ZERO )
                   IF    ( SODR-SRCKBN =   "1" OR "2" )
                    AND  ( FLG-BUNBEN-NYUIN    NOT =   ZERO )
                       PERFORM 2101-ORCSI02-CALL-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-END  =   ZERO )
                   IF    ( SODR-PROCKBN    =   "1" )
                       PERFORM 2101-ORDER-RECONF-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-END =   ZERO )
                   IF    ( SODR-PROCKBN    =   "2" )
                       CONTINUE
                   ELSE
                       PERFORM 2101-ORDER-INSERT-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-END  =   ZERO )
                   IF    ( SODR-KEYGENERATE
                                           =   CONST-API )
                       PERFORM 2101-USERPG-CALL-SEC
      *
                       PERFORM 2101-PUSHEVENT-SEC
                   END-IF
               END-IF
      *
           ELSE
      *
               IF    ( SODR-PROCKBN    =   "1" OR "2" )
                       CONTINUE
               ELSE
                   IF    ( FLG-END  =   ZERO )
                       PERFORM 2101-ORDER-INSERT-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC              SECTION.
      *
           MOVE    CONST-FALSE     TO  ITEM-FORCE-UPDATE
      *
           PERFORM VARYING IDX-ODR    FROM    1   BY  1
                   UNTIL ( IDX-ODR    >   LNK-ODRITEM-MAX )
      *
               EVALUATE    LNK-ODRITEM-ITEMID (IDX-ODR)
               WHEN    NCLAIMID-FORCE-UPDATEJMA
                   PERFORM 20011-FORCE-UPDATE-CHK-SEC
               WHEN    NCLAIMID-TAIINCDJMA
                   PERFORM 20011-TAIINCD-CHK-SEC
               WHEN    NCLAIMID-NYUINKAJMA
                   PERFORM 20011-NYUINKA-CHK-SEC
               WHEN    NCLAIMID-HKNCOMBIJMA
                   PERFORM 20011-HKNCOMBI-CHK-SEC
               END-EVALUATE
      *
           END-PERFORM
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    強制更新区分チェック処理
      *****************************************************************
       20011-FORCE-UPDATE-CHK-SEC      SECTION.
      *
           EVALUATE    FUNCTION UPPER-CASE(LNK-ODRITEM-ITEM(IDX-ODR))
           WHEN    SPACE
           WHEN    CONST-FALSE
               MOVE    CONST-FALSE     TO  ITEM-FORCE-UPDATE
           WHEN    CONST-TRUE
               MOVE    CONST-TRUE      TO  ITEM-FORCE-UPDATE
           WHEN    OTHER
               MOVE    "0197"          TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-EVALUATE
      *
           .
       20011-FORCE-UPDATE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院事由チェック処理
      *****************************************************************
       20011-TAIINCD-CHK-SEC           SECTION.
      *
           COMPUTE CNT-TAIINCD     =   CNT-TAIINCD +   1
      *
           IF    ( CNT-TAIINCD     =   1 )
      *
               IF    ( LNK-ODRITEM-ITEM (IDX-ODR)   =   SPACE )
                AND  ( LNK-ODRITEM-MEMO (IDX-ODR)   =   SPACE )
                   CONTINUE
               ELSE
                   PERFORM 200111-TAIINCD-CHK-SEC
               END-IF
           ELSE
      *
                   MOVE    "0047"  TO   ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       20011-TAIINCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院事由チェック処理
      *****************************************************************
       200111-TAIINCD-CHK-SEC          SECTION.
      *
           MOVE    "5013"          TO  WRK-SYS-KANRICD
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  WRK-SYS-KBNCD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-TAIINCD
           ELSE
               MOVE    "0046"      TO   ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       200111-TAIINCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院科チェック処理
      *****************************************************************
       20011-NYUINKA-CHK-SEC           SECTION.
      *
           COMPUTE CNT-NYUINKA  =   CNT-NYUINKA  +   1
      *
           IF    ( CNT-NYUINKA  =   1 )
            AND  ( ITEM-NYUINKA =   SPACE )
      *
               MOVE    "1005"          TO  WRK-SYS-KANRICD
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  WRK-SYS-KBNCD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    MCPDATA-REC TO  SYS-1005-REC
      *
                   MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-NYUINKA
      *
               ELSE
                   MOVE    "0002"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
      *
           END-IF
      *
           .
       20011-NYUINKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       20011-HKNCOMBI-CHK-SEC           SECTION.
      *
           COMPUTE CNT-HKNCOMBI  =   CNT-HKNCOMBI  +   1
      *
           IF    ( CNT-HKNCOMBI  =   1 )
            AND  ( ITEM-HKNCOMBI =   ZERO )
      *
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  WRK-COMBI-HKNCOMBI
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
               IF    ( FLG-HKNCOMBI        =   ZERO )
                   IF    ( COMB-DELKBN NOT =   "1" )
                     AND ( SODR-SRYYMD    >=  COMB-TEKSTYMD )
                     AND ( SODR-SRYYMD    <=  COMB-TEKEDYMD )
      *
                       MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-HKNCOMBI
                   ELSE
                       MOVE    "0005"  TO  ERRCD-ERRCD
                       PERFORM 2100-ITEM-ERR-SET-SEC
                    END-IF
               ELSE
                       MOVE    "0005"  TO  ERRCD-ERRCD
                       PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
      *
           END-IF
      *
           .
       20011-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    項目未処理状態設定処理
      *****************************************************************
       2100-ITEM-UNPROCSTS-SET-SEC        SECTION.
      *
           MOVE    3               TO  LNK-ODRITEM-STATUS (IDX-ODR)
      *
           .
       2100-ITEM-UNPROCSTS-SET-EXT.
           EXIT.
      *****************************************************************
      *    項目チェックエラー設定処理
      *****************************************************************
       2100-ITEM-ERR-SET-SEC           SECTION.
      *
           IF    ( ERRCD-ITEM      =   SPACE )
               MOVE    ERRCD-ERRCD TO  ERRCD-ITEM
           END-IF
      *
           IF  ( LNK-ODRITEM-STATUS (IDX-ODR)  =   SPACE )
               MOVE    2       TO  LNK-ODRITEM-STATUS (IDX-ODR)
           END-IF
      *
           MOVE    ERRCD-ERRCD TO  LNK-ODRITEM-ERRCD (IDX-ODR)
      *
           .
       2100-ITEM-ERR-SET-EXT.
           EXIT.
      *****************************************************************
      *    項目チェック警告エラー設定処理
      *****************************************************************
       2100-ITEM-WRN-SET-SEC           SECTION.
      *
           IF    ( LNK-ODRITEM-STATUS (IDX-ODR)    =   SPACE )
               MOVE    1           TO  LNK-ODRITEM-STATUS (IDX-ODR)
           END-IF
      *
           MOVE    ERRCD-ERRCD     TO  LNK-ODRITEM-ERRCD (IDX-ODR)
      *
           .
       2100-ITEM-WRN-SET-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2002-KANREN-CHK-SEC             SECTION.
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        入院科チェック処理
               PERFORM 20021-NYUINKA-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        保険組合せチェック処理
               PERFORM 20021-HKNCOMBI-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        定期請求処理中チェック
               PERFORM 20021-TEIKI-JOB-CHK-SEC
               IF    ( FLG-TEIKIJOB    =   1 )
                   MOVE    "0048"      TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        入院履歴チェック処理
               PERFORM 20021-NYUINRRK-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
               IF    ( FLG-SYUARI      =   1 )
                   IF    ( SODR-INF-DIALOG >=  1 )
                       CONTINUE
                   ELSE
                       MOVE    1           TO  WRK-DIALOG
                       MOVE    "0049"      TO  ERRCD-ORDER
                   END-IF
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
               PERFORM 20021-SRYACCT-CHK-SEC
               IF    ( FLG-ACCTARI     =   1 )
                   IF    ( SODR-INF-DIALOG >=  2 )
                       CONTINUE
                   ELSE
                       MOVE    2           TO  WRK-DIALOG
                       MOVE    "0050"      TO  ERRCD-ORDER
                   END-IF
               END-IF
           END-IF
      *
           .
       2002-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院科チェック処理
      *****************************************************************
       20021-NYUINKA-CHK-SEC          SECTION.
      *
           IF    ( ITEM-NYUINKA    =   SPACE )
               MOVE    "0002"      TO  ERRCD-ORDER
           END-IF
      *
           .
       20021-NYUINKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       20021-HKNCOMBI-CHK-SEC          SECTION.
      *
           IF    ( ITEM-HKNCOMBI   =   ZERO )
               MOVE    "0005"      TO  ERRCD-ORDER
           END-IF
      *
           .
       20021-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求処理中チェック
      *****************************************************************
       20021-TEIKI-JOB-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TEIKIJOB
      *
      *    ジョブ管理チェック処理
           INITIALIZE                  STEIKIJOB-AREA
           CALL    "ORCSTEIKIJOB"      USING   STEIKIJOB-AREA
                                               SPA-AREA
           IF    ( STEIKIJOB-ERRCD NOT =   SPACE )
                   MOVE    1           TO  FLG-TEIKIJOB
           END-IF
      *
           .
       20021-TEIKI-JOB-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴チェック処理
      *****************************************************************
       20021-NYUINRRK-CHK-SEC          SECTION.
      *
           MOVE    TRRK-REC        TO  PTNYUINRRK-REC
      *
      *        診療科と保険組合せの変更は不可
           IF    ( PTNYUINRRK-NYUINKA      NOT =   ITEM-NYUINKA )
               MOVE    "0069"  TO  ERRCD-ORDER
               GO  TO  20021-NYUINRRK-CHK-EXT
           END-IF
      *
           IF    ( PTNYUINRRK-HKNCOMBINUM  NOT =   ITEM-HKNCOMBI )
               MOVE    "0070"  TO  ERRCD-ORDER
               GO  TO  20021-NYUINRRK-CHK-EXT
           END-IF
      *
      *    入院中であること
           IF    ( PTNYUINRRK-TAIINYMD NOT =   "99999999" )
               MOVE    "0052"      TO  ERRCD-ORDER
               GO  TO  20021-NYUINRRK-CHK-EXT
           END-IF
      *
           IF    ( PTNYUINRRK-NYUINYMD     >  SODR-SRYYMD )
               MOVE    "0053"  TO  ERRCD-ORDER
               GO  TO  20021-NYUINRRK-CHK-EXT
           END-IF
      *
           IF    ( PTNYUINRRK-TENNYUYMD    >  SODR-SRYYMD )
               MOVE    "0054"  TO  ERRCD-ORDER
               GO  TO  20021-NYUINRRK-CHK-EXT
           END-IF
      *
      *    同日再入院の場合、入院日≠退院日となっていること
           IF    ( PTNYUINRRK-DOUJITSUKBN   =   "1" OR "2" )
               IF    ( PTNYUINRRK-NYUINYMD  =   SODR-SRYYMD )
                   MOVE    "0064"  TO  ERRCD-ORDER
                   GO  TO  20021-NYUINRRK-CHK-EXT
               END-IF
           END-IF
      *
      *    定期請求との関連チェック
      *    請求開始日取得処理
           PERFORM 20021-SKYSTYMD-GET-SEC
           IF    ( ERRCD-ORDER     NOT =   SPACE )
               GO  TO  20021-NYUINRRK-CHK-EXT
           END-IF
      *
      *    月途中の定期請求終了後...(1)、
      *    定期請求区分を「2 月末時のみ請求」に変更された場合、
      *    (1)で作成された収納が未入金であること
           IF    ( PTNYUINRRK-TEIKI-SEIKYUKBN  =   "2" )
             AND ( FLG-TEIKIARI                =   1 )
             AND ( WRK-SKYSTYMD            NOT =   "01" )
      *
               PERFORM 20021-TEIKI-SYU-CHK-SEC
               IF    ( ERRCD-ORDER         NOT =   SPACE )
                   GO  TO  20021-NYUINRRK-CHK-EXT
               END-IF
      *
           END-IF
      *
      *    退院月に会計テーブルが存在していること
           PERFORM 900-NYUINACCT-KEY17-SEL-SEC
           IF    ( FLG-NYUINACCT   NOT =   ZERO )
               MOVE    "0056"          TO  ERRCD-ORDER
           END-IF
      *
           .
       20021-NYUINRRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求期間開始日取得処理
      *****************************************************************
       20021-SKYSTYMD-GET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TEIKIARI
      *
           MOVE    SPACE               TO  WRK-SKYSTYMD
      *
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  PTTEIKIRRK-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  PTTEIKIRRK-PTID
           MOVE    PTNYUINRRK-RRKNUM   TO  PTTEIKIRRK-RRKNUM
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
               IF    ( PTTEIKIRRK-SKYEDYMD
                                       >=  SODR-SRYYMD )
                   MOVE    "0055"      TO  ERRCD-ORDER
               ELSE
                   MOVE    PTTEIKIRRK-SKYEDYMD
                                       TO  WRK-KIJUNYMD
                   MOVE    "1"         TO  WRK-ZOGENPTN
                   MOVE    1           TO  WRK-ZOGEN
                   PERFORM 800-CALENDAR-SEC
                   IF    ( PTNYUINRRK-TEIKI-SEIKYUKBN   =   "2" )
                       IF   ( PTNYUINRRK-NYUINYMD (1 : 6)
                                       <   WRK-KEISANYMD    (1 : 6))
                           MOVE    WRK-KEISANYMD (1 : 6)
                                           TO  WRK-SKYSTYMD (1 : 6)
                           MOVE    "01"    TO  WRK-SKYSTYMD (7 : 2)
                       END-IF
                   ELSE
                       MOVE    WRK-KEISANYMD
                                       TO  WRK-SKYSTYMD
                   END-IF
                   MOVE    1           TO  FLG-TEIKIARI
               END-IF
           END-IF
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( ERRCD-ORDER         =   SPACE )
               IF    ( WRK-SKYSTYMD        =   SPACE )
                   MOVE    PTNYUINRRK-NYUINYMD
                                           TO  WRK-SKYSTYMD
                   IF    ( WRK-SKYSTYMD    <   CONST-H180401 )
                       IF    ( PTNYUINRRK-DOUJITSUKBN  =   "1" )
                           MOVE    WRK-SKYSTYMD    TO  WRK-KIJUNYMD
                           MOVE    "1"             TO  WRK-ZOGENPTN
                           MOVE    1               TO  WRK-ZOGEN
                           PERFORM 800-CALENDAR-SEC
                           MOVE    WRK-KEISANYMD   TO  WRK-SKYSTYMD
                       END-IF
                   END-IF
               END-IF
      *
      *        前回定期請求日の翌日（あるいは入院日）
      *        〜 算定開始日の翌月までの
      *        日付が入力されていること
      *        強制更新="TRUE"の場合、定期請求未実施でも退院請求を
      *        作成する
               MOVE    SODR-SRYYMD         TO  WRK-KIJUNYMD
               MOVE    "2"                 TO  WRK-ZOGENPTN
               MOVE    -1                  TO  WRK-ZOGEN
               PERFORM 800-CALENDAR-SEC
               IF    ( WRK-KEISANYMD (1 : 6)   >   WRK-SKYSTYMD (1 : 6))
                   IF    ( ITEM-FORCE-UPDATE   =   CONST-TRUE )
      *
                       MOVE    WRK-SKYSTYMD    TO  SODR-UNSKYSTYMD
      *
                       MOVE    WRK-KEISANYMD   TO  WRK-SKYSTYMD
                       MOVE    "01"            TO  WRK-SKYSTYMD(7:2)
      *
                       MOVE    WRK-SKYSTYMD    TO  WRK-KIJUNYMD
                       MOVE    "1"             TO  WRK-ZOGENPTN
                       MOVE    -1              TO  WRK-ZOGEN
                       PERFORM 800-CALENDAR-SEC
                       MOVE    WRK-KEISANYMD   TO  SODR-UNSKYEDYMD
      *
                   ELSE
                       MOVE    "0198"          TO  ERRCD-ORDER
                   END-IF
               END-IF
           END-IF
      *
           MOVE    WRK-SKYSTYMD            TO  SODR-SKYSTYMD
      *
           .
       20021-SKYSTYMD-GET-EXT.
           EXIT.
      *****************************************************************
      *    定期請求で作成された収納をチェック
      *****************************************************************
       20021-TEIKI-SYU-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE                  SYUNOU-REC
           MOVE    PTNYUINRRK-HOSPNUM   TO  SYU-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  SYU-PTID
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    WRK-SKYSTYMD        TO  SYU-SKYSTYMD
           MOVE    SODR-SRYYMD         TO  SYU-SKYEDYMD
           MOVE    PTNYUINRRK-RRKNUM   TO  SYU-NYUIN-RRKNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           PERFORM UNTIL   ( FLG-SYUNOU    NOT =   ZERO )
                    OR     ( ERRCD-ORDER   NOT =   SPACE )
      *
               IF    ( SYU-DENPJTIKBN  NOT =   "3" )
                   MOVE    1           TO  FLG-SYUARI
                   IF    ( SYU-NYUKIN-TOTAL    >   ZERO )
                       MOVE    "0058"  TO  ERRCD-ORDER
                   END-IF
               END-IF
      *
               MOVE    "tbl_syunou"    TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  SYUNOU-REC
               ELSE
                   MOVE    1           TO  FLG-SYUNOU
                   INITIALIZE          SYUNOU-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20021-TEIKI-SYU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療会計チェック処理
      *****************************************************************
       20021-SRYACCT-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ACCTARI
      *
           MOVE    SODR-SRYYMD         TO  WRK-TAIINYMD
      *
           MOVE    ZERO                TO  FLG-SRYACCT
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SODR-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    "1"                 TO  ACCT-NYUGAIKBN
           MOVE    SODR-PTID           TO  ACCT-PTID
           MOVE    WRK-TAIINYM         TO  ACCT-SRYYM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC 
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SRYACCT     NOT =   ZERO )
                   OR    ( FLG-ACCTARI     NOT =   ZERO )
      *
               IF    ( ACCT-SRYCDTOTAL =   120001110
                  OR                       120000410 )
                   CONTINUE
               ELSE
                   IF    ( WRK-TAIINYM     =   ACCT-SRYYM )
                       COMPUTE IDX0        =   WRK-TAIINDD +   1
                   ELSE
                       COMPUTE IDX0        =   1
                   END-IF
      *
                   PERFORM UNTIL ( IDX0               >   31 )
                           OR    ( FLG-ACCTARI    NOT =   ZERO )
      *
                       IF    ( ACCT-DAY (1 IDX0)      >   ZERO )
                           MOVE    1       TO  FLG-ACCTARI
                       END-IF
      *
                       COMPUTE IDX0        =   IDX0    +   1
      *
                   END-PERFORM
               END-IF
      *
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    "key15"         TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  SRYACCT-REC
               ELSE
                   MOVE    1           TO  FLG-SRYACCT
                   INITIALIZE              SRYACCT-REC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20021-SRYACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    収納情報編集処理
      *****************************************************************
       2101-SYUNOU-HEN-SEC             SECTION.
      *
           MOVE    LNK-SYUNOU-REC (IDX-SYU)    TO  SYUNOU-REC
      *
           IF    ( WRK-SKYSTYMD  (1:6) NOT =   SODR-SRYYMD (1:6) )
               IF    ( WRK-SKYSTYMD (1:6)  =   SYU-SKYSTYMD (1:6))
                   MOVE    WRK-SKYSTYMD        TO  SYU-SKYSTYMD
                   MOVE    WRK-SKYSTYMD        TO  WRK-SYMD
                   PERFORM 800-GETUMATU-SEC
                   MOVE    WRK-SYMD            TO  SYU-SKYEDYMD
               END-IF
               IF    ( SODR-SRYYMD (1:6)   =   SYU-SKYEDYMD (1:6) )
                   MOVE    SODR-SRYYMD (1:6)   TO  SYU-SKYSTYMD(1:6)
                   MOVE    "01"                TO  SYU-SKYSTYMD(7:2)
                   MOVE    SODR-SRYYMD         TO  SYU-SKYEDYMD
               END-IF
           ELSE
               MOVE    WRK-SKYSTYMD            TO  SYU-SKYSTYMD
               MOVE    SODR-SRYYMD             TO  SYU-SKYEDYMD
           END-IF
      *
      *    請求時最終履歴取得処理
           PERFORM 21011-SEILASTRRK-SET-SEC
           IF    ( ERRCD-ORDER     NOT =   SPACE )
               GO  TO  2101-SYUNOU-HEN-EXT
           END-IF
      *
           MOVE    WRK-NYUINKA     TO  SYU-SRYKA
           MOVE    WRK-BRMNUM      TO  SYU-ROOMNUM
           MOVE    WRK-BTUNUM      TO  SYU-BYOTONUM
           MOVE    WRK-DRCD        TO  SYU-DRCD
      *
           IF    ( SYU-SKYMONEY        =   ZERO )
               MOVE    SPACE       TO  SYU-DENPJTIKBN
           ELSE
               IF    ( SYU-SKYMONEY   <=  SYU-NYUKIN-TOTAL )
                   MOVE    "1"     TO  SYU-DENPJTIKBN
               ELSE
                   MOVE    "2"     TO  SYU-DENPJTIKBN
               END-IF
           END-IF
      *
           MOVE    TRRK-RRKNUM     TO  SYU-NYUIN-RRKNUM
      *
           MOVE    SYUNOU-REC      TO  LNK-SYUNOU-REC (IDX-SYU)
      *
           .
       2101-SYUNOU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    請求時最終履歴取得処理
      *****************************************************************
       21011-SEILASTRRK-SET-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-NYUINKA
                                           WRK-BTUNUM
                                           WRK-BRMNUM
                                           WRK-DRCD
      *
      *    入院履歴マスタ検索
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SODR-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SODR-PTID           TO  PTNYUINRRK-PTID
           MOVE    TRRK-RRKNUM         TO  PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( WRK-NYUINKA     NOT =   SPACE )
      *
               IF    ( PTNYUINRRK-TENNYUYMD
                                       <=  SYU-SKYEDYMD )
                AND  ( PTNYUINRRK-TENSTUYMD
                                       >=  SYU-SKYEDYMD )
                   MOVE    PTNYUINRRK-NYUINKA
                                       TO  WRK-NYUINKA
                   MOVE    PTNYUINRRK-BTUNUM
                                       TO  WRK-BTUNUM
                   MOVE    PTNYUINRRK-BRMNUM
                                       TO  WRK-BRMNUM
                   MOVE    PTNYUINRRK-DRCD1 (2 : 4)
                                       TO  WRK-DRCD
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
               ELSE
                   MOVE    1           TO  FLG-PTNYUINRRK
                   INITIALIZE              PTNYUINRRK-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( WRK-NYUINKA         =   SPACE )
               MOVE    "0059"          TO  ERRCD-ORDER
           END-IF
      *
           .
       21011-SEILASTRRK-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院負担金計算処理
      *****************************************************************
       2101-ORCSNYUFTN-CALL-SEC        SECTION.
      *
      *    通算日数を求める
           MOVE    TRRK-NYUINYMD   TO  WRK-STYMD
      *
      *    請求開始日（入院日または前回定期請求終了日の翌日）を設定
           MOVE    WRK-SKYSTYMD    TO  WRK-EDYMD
           PERFORM 800-KIKAN-CALC-SEC
           IF    ( FLG-ORCSDAY     NOT =   ZERO )
               MOVE    "0060"      TO  ERRCD-ORDER
               GO  TO  2101-ORCSNYUFTN-CALL-EXT
           END-IF
      *
           INITIALIZE                  LNK-ORCS02-REC
                                       LNK-SYUNOU-REC-T
                                       LNK-SYUDAY-REC-T
      *
           INITIALIZE                  SS006-AREA
           INITIALIZE                  SS006-AREA
           MOVE    WRK-SKYSTYMD        TO  SS006-SKYSTYMD
           MOVE    SODR-SRYYMD         TO  SS006-SKYEDYMD
           MOVE    "1"                 TO  SS006-SKYKBN
           CALL    "ORCSS006"      USING
                                   SS006-AREA
                                   SPA-AREA
      *
           MOVE    SODR-HOSPNUM     TO  LNK-S02-HOSPNUM
           MOVE    "1"             TO  LNK-S02-NYUGAIKBN
           MOVE    SODR-PTID       TO  LNK-S02-PTID
           MOVE    WRK-NISSU1      TO  LNK-S02-NYUIN-TSUSAN-NISSU
      *
      *    請求開始日（入院日または前回定期請求終了日の翌日）を設定
           MOVE    WRK-SKYSTYMD    TO  LNK-S02-SKYSTYMD
           MOVE    SODR-SRYYMD     TO  LNK-S02-SKYEDYMD
      *
           IF    ( WRK-SKYSTYMD    <   CONST-H180401 )
               CONTINUE
           ELSE
               MOVE    SS006-DOJITUFLG-ST  TO  LNK-S02-DOJITUFLG-ST
               MOVE    SS006-TIMEKBN-ST    TO  LNK-S02-TIMEKBN-ST
           END-IF
           IF    ( SODR-SRYYMD     <   CONST-H180401 )
               CONTINUE
           ELSE
               MOVE    SS006-DOJITUFLG-ED  TO  LNK-S02-DOJITUFLG-ED
               MOVE    SS006-TIMEKBN-ED    TO  LNK-S02-TIMEKBN-ED
           END-IF
      *
           MOVE    SPA-SYSYMD      TO  LNK-S02-SYSYMD
           MOVE    "1"             TO  LNK-S02-SYORI-KBN
           CALL    "ORCSNYUFTN"    USING
                                   SPA-AREA
                                   LNK-ORCS02-REC
                                   LNK-SYUNOU-REC-T
                                   LNK-SYUDAY-REC-T
           IF    ( LNK-S02-RC      = ZERO )
               IF    ( LNK-SYU-SKYSTYMD(1) =   SPACE )
                   MOVE    "0061"      TO  ERRCD-ORDER
               END-IF
           ELSE
               EVALUATE LNK-S02-RC
               WHEN    37
                   MOVE    "7037"      TO  ERRCD-ORDER
               WHEN    OTHER
                   MOVE    "0061"      TO  ERRCD-ORDER
               END-EVALUATE
           END-IF
      *
           .
       2101-ORCSNYUFTN-CALL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴更新処理
      *****************************************************************
       2101-PTNYUINRRK-UPDATE-SEC      SECTION.
      *
           MOVE    TRRK-REC        TO  PTNYUINRRK-REC
           MOVE    SODR-SRYYMD     TO  PTNYUINRRK-TAIINYMD
                                       PTNYUINRRK-TENSTUYMD
      *
           MOVE    ITEM-TAIINCD    TO  PTNYUINRRK-TAIINCD
      *
           MOVE    SODR-OPID       TO  PTNYUINRRK-OPID
           MOVE    SODR-UPYMD      TO  PTNYUINRRK-UPYMD
           MOVE    SODR-UPHMS      TO  PTNYUINRRK-UPHMS
      *
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "upd2"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               DISPLAY "ORCSDR02 ERROR 001:PTNYUINRRK UPDATE ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  2101-PTNYUINRRK-UPDATE-EXT
           END-IF
      *
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "upd3"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               DISPLAY "ORCSDR02 ERROR 002:PTNYUINRRK UPDATE ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  2101-PTNYUINRRK-UPDATE-EXT
           END-IF
      *
           MOVE    PTNYUINRRK-REC      TO  TRRK-REC
      *
           .
       2101-PTNYUINRRK-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    請求取消更新処理
      *****************************************************************
       2101-TORIKESI-KOUSHIN-SEC       SECTION.
      *
      *    患者定期請求履歴を更新する
           IF    ( FLG-END         =   ZERO )
               PERFORM 2101-PTTEIKIRRK-UPD-SEC
           END-IF
      *
      *    収納データを更新する
           IF    ( FLG-END         =   ZERO )
               PERFORM 2101-SYUNOU-UPD-SEC
           END-IF
      *
           .
       2101-TORIKESI-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *    収納データ更新処理
      *****************************************************************
       2101-SYUNOU-UPD-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE  SYUNOU-REC
           MOVE    SODR-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SODR-PTID           TO  SYU-PTID
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    WRK-SKYSTYMD        TO  SYU-SKYSTYMD
           MOVE    SODR-SRYYMD         TO  SYU-SKYEDYMD
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
      *
               IF    ( SYU-NYUIN-RRKNUM    =   TRRK-RRKNUM )
                AND  ( SYU-DENPJTIKBN  NOT =   "3" )
      *
                   COMPUTE WRK-SKYMONEY
                       =   SYU-SKYMONEY        *   -1
      *
      *            請求取消として収納データを更新
                   MOVE    ZERO            TO  SYU-SKYMONEY
                   MOVE    "3"             TO  SYU-DENPJTIKBN
      *
      *            収納明作成処理
                   PERFORM 2101-SYUMEI-SAKUSEI-SEC
                   IF    ( FLG-END     =   ZERO )
      *
                       MOVE    SODR-OPID       TO  SYU-OPID
                       MOVE    SODR-UPYMD      TO  SYU-UPYMD
                       MOVE    SODR-UPHMS      TO  SYU-UPHMS
      *
                       MOVE    SYUNOU-REC      TO  MCPDATA-REC
                       MOVE    "tbl_syunou"    TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBUPDATE-SEC
                       IF    ( MCP-RC      NOT =   ZERO )
                          DISPLAY "ORCSDR02 ERROR 003:SYUNOU UPDATE ERR"
                           " PTID="        SODR-PTID
                           " KARTE-KEY="   SODR-KARTE-KEY
                           MOVE    1           TO  FLG-END
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_syunou"    TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC     TO  SYUNOU-REC
               ELSE
                   MOVE    1               TO  FLG-SYUNOU
                   INITIALIZE                  SYUNOU-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"            TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       2101-SYUNOU-UPD-EXT.
           EXIT.
      *****************************************************************
      *    収納明細作成処理
      *****************************************************************
       2101-SYUMEI-SAKUSEI-SEC         SECTION.
      *
           PERFORM 800-SS002-SEC
      *
      *    伝票番号枝番を求める
           MOVE    ZERO                TO  FLG-SYUMEI
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    ZERO            TO  FLG-SYUMEI
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
               INITIALIZE                  SYUMEI-REC
           END-IF
      *
           COMPUTE WRK-DENPEDANUM  =   SMEI-DENPEDANUM +   1
      *
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *伝票番号枝番
           MOVE    WRK-DENPEDANUM      TO  SMEI-DENPEDANUM
      *入金連番
           MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *診療科
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *請求発行日（発行日の西暦）
           MOVE    SPACE               TO  SMEI-SKYPRINTYMD
      *請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *領収書発行日
           MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *請求金額
           MOVE    WRK-SKYMONEY        TO  SMEI-SKYMONEY
      *
      *入返金区分
           MOVE    SPACE               TO  SMEI-NYUHEN-KBN
      *入返金額
           MOVE    ZERO                TO  SMEI-NYUHEN-MONEY
      *入返金日
           MOVE    SODR-UKEYMD         TO  SMEI-NYUHEN-YMD
      *入金方法
           MOVE    "00"                TO  SMEI-NYUKIN-HOHO
      *状態区分
           MOVE    "5"                 TO  SMEI-JOUTAIKBN
      *
      *    追加対象のデータが既に存在しないかチェック
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               DISPLAY "ORCSDR02 ERROR 004:SYUMEI INSERT CHK ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  2101-SYUMEI-SAKUSEI-EXT
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    SODR-OPID           TO  SMEI-OPID
           MOVE    SODR-UPYMD          TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SODR-UPHMS          TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           PERFORM 800-SET-SS002-SMEI-SEC
      *
      *    収納明細追加
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "ORCSDR02 ERROR 005:SYUMEI INSERT ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       2101-SYUMEI-SAKUSEI-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴更新処理
      *****************************************************************
       2101-PTTEIKIRRK-UPD-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
      *
      *    患者請求履歴更新
           INITIALIZE  PTTEIKIRRK-REC
           MOVE    SODR-HOSPNUM         TO  PTTEIKIRRK-HOSPNUM
           MOVE    SODR-PTID           TO  PTTEIKIRRK-PTID
           MOVE    WRK-SKYSTYMD        TO  PTTEIKIRRK-SKYSTYMD
           MOVE    SODR-SRYYMD         TO  PTTEIKIRRK-SKYEDYMD
           MOVE    TRRK-RRKNUM         TO  PTTEIKIRRK-RRKNUM
           MOVE    SODR-UPYMD          TO  PTTEIKIRRK-UPYMD
           MOVE    SODR-UPHMS          TO  PTTEIKIRRK-UPHMS
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "upd2"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "ORCSDR02 ERROR 006:PTTEIKIRRK UPDATE ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       2101-PTTEIKIRRK-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    オーダー情報再編成処理
      *****************************************************************
       2101-ORDER-RECONF-SEC           SECTION.
      *
           CALL    "ORCSODRS01"        USING   SODR-AREA
                                               SODRS01-AREA
                                               LNK-ODRITEM-REC
                                               MCPAREA
                                               SPA-AREA
           MOVE    SODRS01-RC      TO  FLG-END
      *
           .
       2101-ORDER-RECONF-EXT.
           EXIT.
      *****************************************************************
      *    オーダー追加処理
      *****************************************************************
       2101-ORDER-INSERT-SEC           SECTION.
      *
           PERFORM 21011-ORDER-HEN-SEC
      *
           PERFORM VARYING IDX-ODR FROM    1   BY  1
                   UNTIL ( IDX-ODR >   LNK-ODRITEM-MAX )
                    OR   ( FLG-END =   1 )
      *
                PERFORM 21011-ODRITEM-HEN-SEC
      *
           END-PERFORM
      *
           .
       2101-ORDER-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    ユーザプログラム実行処理
      *****************************************************************
       2101-USERPG-CALL-SEC            SECTION.
      *
           INITIALIZE                      ORCSUSERCHKAREA
           MOVE    "I04"               TO  ORCSUSERCHK-GMNPG
           MOVE    "2"                 TO  ORCSUSERCHK-SYORI
           MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SYSYMD
           MOVE    TRRK-TAIINYMD       TO  ORCSUSERCHK-SRYYMD
           MOVE    SODR-PTID           TO  ORCSUSERCHK-PTID
           MOVE    SODR-PTNUM          TO  ORCSUSERCHK-PTNUM
           MOVE    TRRK-NYUINKA        TO  ORCSUSERCHK-SRYKA
           MOVE    TRRK-HKNCOMBINUM    TO  ORCSUSERCHK-HKNCOMBI
           MOVE    TRRK-DRCD1          TO  ORCSUSERCHK-DRCD
           MOVE    02                  TO  ORCSUSERCHK-SYORI-FLG
           CALL    "ORCSUSERCHK"           USING
                                           ORCSUSERCHKAREA
                                           SPA-AREA
      *
           .
       2101-USERPG-CALL-EXT.
           EXIT.
      *****************************************************************
      *    PUSH通知処理
      *****************************************************************
       2101-PUSHEVENT-SEC            SECTION.
      *
           INITIALIZE                      SI03-AREA
      *
           MOVE    "02"                TO  SI03-REQNUM
           MOVE    SODR-PTNUM          TO  SI03-PTNUM
           MOVE    TRRK-NYUINYMD       TO  SI03-NYUINYMD
           MOVE    TRRK-TAIINYMD       TO  SI03-TAIINYMD
           CALL    "ORCSI03"           USING
                                       SI03-AREA
                                       SPA-AREA
      *
           .
       2101-PUSHEVENT-EXT.
           EXIT.
      *****************************************************************
      *    オーダ編集処理
      *****************************************************************
       21011-ORDER-HEN-SEC             SECTION.
      *
           INITIALIZE                      ORDER-REC
      *
           MOVE   SODR-HOSPNUM          TO  ORDER-HOSPNUM
           MOVE   SODR-KARTE-KEY       TO  ORDER-KARTE-KEY
           MOVE   SODR-ORDERNUM        TO  ORDER-ORDERNUM
           MOVE   SODR-NYUGAIKBN       TO  ORDER-NYUGAIKBN
           MOVE   SODR-PTNUM           TO  ORDER-PTNUM
           MOVE   SODR-PTID            TO  ORDER-PTID
           MOVE   SODR-SRYKA           TO  ORDER-SRYKA
           MOVE   SODR-SRYYMD          TO  ORDER-SRYYMD
           MOVE   SODR-HKNCOMBI        TO  ORDER-HKNCOMBI
           MOVE   SODR-ORDERID         TO  ORDER-ORDERID
           MOVE   SODR-UKEYMD          TO  ORDER-UKEYMD
           MOVE   SODR-UKEHMS          TO  ORDER-UKEHMS
           MOVE   SODR-KEYGENERATE     TO  ORDER-KEYGENERATE
      *
           MOVE   ERRCD-ORDER          TO  ORDER-ERRCD1
           MOVE   ERRCD-ITEM           TO  ORDER-ERRCD2
      *
           IF    ( ERRCD-ITEM          =   SPACE )
            AND  ( ERRCD-ORDER         =   SPACE )
               MOVE    TRRK-RRKNUM     TO  ORDER-RRKNUM
               MOVE    TRRK-RRKEDANUM  TO  ORDER-RRKEDANUM
           ELSE
               IF    ( WRK-DIALOG      =   ZERO )
                   MOVE    2           TO  ORDER-STATUS
               ELSE
                   MOVE    5           TO  ORDER-STATUS
               END-IF
           END-IF
      *
           IF    ( SODR-PROCKBN    =   "1" )
               MOVE     1              TO  ORDER-MODIFY
           END-IF
      *
           MOVE    SODR-OPID           TO  ORDER-OPID
           MOVE    SODR-UPYMD          TO  ORDER-CREYMD
                                           ORDER-UPYMD
           MOVE    SODR-UPHMS          TO  ORDER-UPHMS
      *
      *
           PERFORM 900-ORDER-KEY-SEL-SEC
           IF    ( FLG-ORDER           =   ZERO )
               DISPLAY "ORCSDR02 ERROR 007:ORDER INSERT CHK ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  21011-ORDER-HEN-EXT
           END-IF
      *
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "ORCSDR02 ERROR 008:ORDER INSERT ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       21011-ORDER-HEN-EXT.
           EXIT.
      *****************************************************************
      *    オーダ項目編集処理
      *****************************************************************
       21011-ODRITEM-HEN-SEC           SECTION.
      *
           MOVE    LNK-ODRITEM-OCC (IDX-ODR)
                                       TO  ODRITEM-REC
      *
           IF    ( LNK-ODRITEM-ITEMID (IDX-ODR)
                                       =   NCLAIMID-TAIINCDJMA
                                       OR  NCLAIMID-NYUINKAJMA
                                       OR  NCLAIMID-HKNCOMBIJMA
                                           )
               CONTINUE
           ELSE
               MOVE    3               TO  ODRITEM-STATUS
               MOVE    "9001"          TO  ODRITEM-ERRCD
           END-IF
      *
           MOVE    SODR-OPID           TO  ODRITEM-OPID
           MOVE    SODR-UPYMD          TO  ODRITEM-CREYMD
                                           ODRITEM-UPYMD
           MOVE    SODR-UPHMS          TO  ODRITEM-UPHMS
      *
      *
           PERFORM 900-ODRITEM-KEY-SEL-SEC
           IF    ( FLG-ODRITEM           =   ZERO )
               DISPLAY "ORCSDR02 ERROR 009:ODRITEM INSERT CHK ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  21011-ODRITEM-HEN-EXT
           END-IF
      *
           MOVE    ODRITEM-REC         TO  MCPDATA-REC
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "ORCSDR02 ERROR 010:ODRITEM INSERT ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       21011-ODRITEM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院会計作成サブ呼出
      *****************************************************************
       2101-ORCSODRNACCT-CALL-SEC      SECTION.
      *
           INITIALIZE                  SODRNACCT-AREA
           MOVE    NORDERID-TAIINID TO SODRNACCT-ORDERID
           MOVE    SODR-OPID       TO  SODRNACCT-OPID
           MOVE    SODR-UPYMD      TO  SODRNACCT-UPYMD
           MOVE    SODR-UPHMS      TO  SODRNACCT-UPHMS
           MOVE    TRRK-REC        TO  LNK-PTNYUINRRK-REC

      *
           CALL    "ORCSODRNACCT"      USING
                                       SODRNACCT-AREA
                                       LNK-PTNYUINRRK-REC
                                       SPA-AREA
           IF    ( SODRNACCT-RC    NOT =   ZERO )
               MOVE    1           TO  FLG-END
           END-IF
      *
           .
       2101-ORCSODRNACCT-CALL-EXT.
           EXIT.
      *****************************************************************
      *    収納更新処理
      *****************************************************************
       2102-SYUNOU-UPD-SEC             SECTION.
      *
           MOVE    LNK-SYUNOU-REC (IDX-SYU)
                                   TO  SYUNOU-REC
      *
      *    伝票番号取得処理
           PERFORM 21021-DENPNUM-GET-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  2102-SYUNOU-UPD-EXT
           END-IF
      *
           MOVE    WRK-DENPNUM     TO  SYU-DENPNUM
      *
           MOVE    WRK-DENPNUM     TO  SODR-DENPNUM (IDX-SYU)
           MOVE    IDX-SYU         TO  SODR-DENPNUM-MAX
      *
      *    更新前チェック（既に重複するデータがある場合はエラー）
           MOVE   ZERO             TO  FLG-SYUNOU
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-SYUNOU      =   ZERO )
               DISPLAY "ORCSDR02 ERROR 013:SYUNOU INSERT CHK ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  2102-SYUNOU-UPD-EXT
           END-IF
      *
           MOVE    SODR-OPID       TO  SYU-OPID
           MOVE    SODR-UPYMD      TO  SYU-CREYMD
                                       SYU-UPYMD
           MOVE    SODR-UPHMS      TO  SYU-UPHMS
      *
           PERFORM 800-SS002-SEC
      *
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               DISPLAY "ORCSDR02 ERROR 014:SYUNOU INSERT ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  2102-SYUNOU-UPD-EXT
           END-IF
      *
      *    日別収納マスタ更新処理
           PERFORM 2102-SYUDAY-UPD-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  2102-SYUNOU-UPD-EXT
           END-IF
      *
      *    収納明細マスタ更新処理
           PERFORM 2102-SYUMEI-UPD-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  2102-SYUNOU-UPD-EXT
           END-IF
      *
           .
       2102-SYUNOU-UPD-EXT.
           EXIT.
      *****************************************************************
      *    日別収納マスタ更新処理
      *****************************************************************
       2102-SYUDAY-UPD-SEC         SECTION.
      *
           MOVE    LNK-SYUDAY-REC (IDX-SYU)
                                       TO  SYUDAY-REC
      *
           IF    ( SDAY-DAYINFFLG      =   "1" )
      *
               MOVE    SYU-DENPNUM     TO  SDAY-DENPNUM
      *
      *        更新前チェック（既に重複するデータがある場合はエラー）
               MOVE    ZERO            TO  FLG-SYUDAY
               MOVE    SYUDAY-REC      TO  MCPDATA-REC
               MOVE    "tbl_syuday"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    1           TO  FLG-SYUDAY
               END-IF
      *
               MOVE    "tbl_syuday"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               IF    ( FLG-SYUDAY      =   ZERO )
                   DISPLAY "ORCSDR02 ERROR 015:SYUDAY INSERT CHK ERR"
                           " PTID="        SODR-PTID
                           " KARTE-KEY="   SODR-KARTE-KEY
                   MOVE    1               TO  FLG-END
                   GO  TO  2102-SYUDAY-UPD-EXT
               END-IF
      *
               MOVE    SODR-OPID       TO  SDAY-OPID
               MOVE    SODR-UPYMD      TO  SDAY-CREYMD
                                           SDAY-UPYMD
               MOVE    SODR-UPHMS      TO  SDAY-UPHMS
      *
               MOVE    SYUDAY-REC      TO  MCPDATA-REC
               MOVE    "tbl_syuday"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   DISPLAY "ORCSDR02 ERROR 016:SYUDAY INSERT ERR"
                           " PTID="        SODR-PTID
                           " KARTE-KEY="   SODR-KARTE-KEY
                   MOVE    1               TO  FLG-END
               END-IF
           END-IF
      *
           .
      *
       2102-SYUDAY-UPD-EXT.
           EXIT.
      *****************************************************************
      *    収納明細マスタ更新処理
      *****************************************************************
       2102-SYUMEI-UPD-SEC             SECTION.
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *
      *    伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *    伝票番号枝番
           MOVE    01                  TO  SMEI-DENPEDANUM
      *    診療科
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *    伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *    請求発行日（発行日の西暦）
           MOVE    SODR-UKEYMD         TO  SMEI-SKYPRINTYMD
      *    請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *    領収書発行日
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-RYOSYUPRINTYMD
      *    領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *    請求金額
           MOVE    SYU-SKYMONEY        TO  SMEI-SKYMONEY
      *    入返金日
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-NYUHEN-YMD
      *    入金連番
           MOVE    01                  TO  SMEI-NYUKINRENNUM
      *
      *    入金額がゼロの時、入金力なし
           IF    ( SYU-NYUKIN-TOTAL    =   ZERO )
               CONTINUE
           ELSE
      *        入返金区分
               MOVE    "1"             TO  SMEI-NYUHEN-KBN
      *        入返金額
               MOVE    SYU-NYUKIN-TOTAL
                                       TO  SMEI-NYUHEN-MONEY
      *        入金方法
               MOVE    "01"            TO  SMEI-NYUKIN-HOHO
           END-IF
      *
      *    状態区分
           IF    ( SYU-SKYMONEY        =   ZERO )
               MOVE    SPACE               TO  SMEI-JOUTAIKBN
           ELSE
               IF   (  SYU-SKYMONEY    >   SYU-NYUKIN-TOTAL )
                   MOVE    "9"             TO  SMEI-JOUTAIKBN
               ELSE
                   MOVE    "1"             TO  SMEI-JOUTAIKBN
               END-IF
           END-IF
      *
      *    更新前チェック（既に重複するデータがある場合はエラー）
           MOVE   ZERO             TO  FLG-SYUMEI
           MOVE    SYUMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    1           TO  FLG-SYUMEI
           END-IF
      *
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-SYUMEI      =   ZERO )
               DISPLAY "ORCSDR02 ERROR 017:SYUMEI INSERT CHK ERR 2"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1           TO  FLG-END
               GO  TO  2102-SYUMEI-UPD-EXT
           END-IF
      *
      *
           MOVE    SODR-OPID       TO  SMEI-OPID
           MOVE    SODR-UPYMD          TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SODR-UPHMS          TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           PERFORM 800-SET-SS002-SMEI-SEC
      *
           MOVE    SYUMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               DISPLAY "ORCSDR02 ERROR 018:SYUMEI INSERT ERR 2"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1           TO  FLG-END
               GO  TO  2102-SYUMEI-UPD-EXT
           END-IF
      *
           .
       2102-SYUMEI-UPD-EXT.
           EXIT.
      *****************************************************************
      *    伝票番号取得処理
      *****************************************************************
       21021-DENPNUM-GET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
                                           WRK-DENPNUM
      *
           INITIALIZE                  SYS-0042-REC
           MOVE    "0042"              TO  SYS-0042-KANRICD
           MOVE    "*"                 TO  SYS-0042-KBNCD
           MOVE    SODR-SRYYMD         TO  SYS-0042-STYUKYMD
                                           SYS-0042-EDYUKYMD
           MOVE    SODR-HOSPNUM        TO  SYS-0042-HOSPNUM
      *
           MOVE    SYS-0042-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-0042-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYS-0042-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYS-0042-DENPNUMMAX-NYU
                                       TO  WRK-DENPNUM-X
               IF    ( WRK-DENPNUM-X   =   SPACE )
                   MOVE    ZERO        TO  SYS-0042-DENPNUMMAX-NYU
               ELSE
                   MOVE    WRK-DENPNUM-X
                                       TO  SYS-0042-DENPNUMMAX-NYU
               END-IF
      *
               COMPUTE SYS-0042-DENPNUMMAX-NYU
                   =   SYS-0042-DENPNUMMAX-NYU +   1
      *
               MOVE    SYS-0042-DENPNUMMAX-NYU
                                           TO  WRK-DENPNUM
      *
               MOVE    SODR-OPID           TO  SYS-0042-OPID
               MOVE    SODR-UPYMD          TO  SYS-0042-UPYMD
               MOVE    SODR-UPHMS          TO  SYS-0042-UPHMS
      *
               MOVE    SYS-0042-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   DISPLAY "ORCSDR02 ERROR 019:DENPNUM UPDATE ERR"
                           " PTID="        SODR-PTID
                           " KARTE-KEY="   SODR-KARTE-KEY
                   MOVE    1               TO  FLG-END
               END-IF
           ELSE
                   DISPLAY "ORCSDR02 ERROR 020:DENPNUM SELECT ERR"
                           " PTID="        SODR-PTID
                           " KARTE-KEY="   SODR-KARTE-KEY
                   MOVE    1               TO  FLG-END
           END-IF
      *
           .
       21021-DENPNUM-GET-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理更新処理
      *****************************************************************
       2101-ORCSI02-CALL-SEC           SECTION.
      *
           INITIALIZE                  SI02-AREA
           MOVE    "4"             TO  SI02-REQID
           MOVE    TRRK-REC        TO  SNRRK-AREA
           CALL    "ORCSI02"       USING
                                   SI02-AREA
                                   SNRRK-AREA
                                   MCPAREA
                                   SPA-AREA
           IF     ( SI02-RC        NOT =  ZERO )
               DISPLAY "ORCSDR02 ERROR 021:BUNBEN UPDATE ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       2101-ORCSI02-CALL-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( ERRCD-ITEM      =   SPACE )
            AND  ( ERRCD-ORDER     =   SPACE )
               MOVE    ZERO        TO  SODR-ERRFLG
           ELSE
               MOVE    1           TO  SODR-ERRFLG
           END-IF
      *
           MOVE    ERRCD-ORDER     TO  SODR-ERRCD
      *
           MOVE    WRK-DIALOG      TO  SODR-DIALOG
      *
           MOVE    FLG-END         TO  SODR-RC
      *
           IF    ( SODR-RC         =   ZERO )
            AND  ( SODR-ERRFLG     =   ZERO )
               MOVE    TRRK-RRKNUM TO  SODR-RRKNUM
               MOVE    TRRK-RRKEDANUM
                                   TO  SODR-RRKEDANUM
               MOVE    TRRK-SHONUM TO  SODR-SHONUM
           END-IF
      *
           IF    ( FLG-END         NOT =   ZERO )
               MOVE    "tbl_ptnyuinrrk"
                                   TO  MCP-TABLE
               MOVE    "abort"     TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
           END-IF
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    半角文字チェック
      *****************************************************************
       800-HANKAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-MAE-INPUT
                                   =   SPACE )
               GO  TO  800-HANKAKU-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    =   ZERO )
               IF      ( KANACHK-SYUBETU   NOT =   1 )
                   MOVE    1       TO  FLG-ENTRYCHK
               END-IF
           ELSE
                   MOVE    1       TO  FLG-ENTRYCHK
           END-IF
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    数値変換処理
      *****************************************************************
       800-ORCSNUM-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSNUM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-SNUM-INX        TO  SNUM-INX
           CALL    "ORCSNUM"           USING   ORCSNUMAREA
           IF    ( SNUM-RC         NOT =   ZERO  )
            OR   ( SNUM-MINKBN     NOT =   SPACE )
            OR   ( SNUM-SYOKBN     NOT =   SPACE )
               MOVE    1               TO  FLG-ORCSNUM
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-SNUM-OUTNUM
           END-IF
      *
           .
      *
       800-ORCSNUM-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-KIJUNYMD        TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    期間算出処理
      *****************************************************************
       800-KIKAN-CALC-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSDAY
                                           WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                           LNK-DAY5-AREA
           IF      ( STS-DAY-RC1       =   ZERO )
               MOVE    LNK-DAY5-NISSU1 TO  WRK-NISSU1
               MOVE    LNK-DAY5-NISSU2 TO  WRK-NISSU2
           ELSE
               MOVE    1               TO  FLG-ORCSDAY
           END-IF
      *
           .
       800-KIKAN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    日報キー取得サブ
      *****************************************************************
       800-DAILYKEY-SEC         SECTION.
      *
           CALL    "ORCSDAILYKEY"   USING   SDAILYKEYAREA
                                            SPA-AREA
      *
           .
      *
       800-DAILYKEY-EXT.
           EXIT.
      *****************************************************************
      *    収納更新差額取得サブ
      *****************************************************************
       800-SS002-SEC           SECTION.
      *
           INITIALIZE                      SS002-AREA
           CALL    "ORCSS002"      USING   SS002-AREA
                                           SYUNOU-REC
                                           SPA-AREA
      *
           .
      *
       800-SS002-EXT.
           EXIT.
      *****************************************************************
      *    収納明細更新差額設定処理
      *****************************************************************
       800-SET-SS002-SMEI-SEC      SECTION.
      *
           MOVE    SS002-SYURRKNUM     TO  SMEI-SYURRKNUM
           MOVE    SS002-SYUEDANUM     TO  SMEI-SYUEDANUM
           MOVE    SS002-SYURRKUPDKBN  TO  SMEI-SYURRKUPDKBN
      *
           .
      *
       800-SET-SS002-SMEI-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYSKANRI-REC
      *
           MOVE    WRK-SYS-KANRICD     TO  SYS-KANRICD
           MOVE    SODR-SRYYMD         TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SODR-HOSPNUM        TO  SYS-HOSPNUM
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYSKANRI-REC
      *
           MOVE    WRK-SYS-KANRICD     TO  SYS-KANRICD
           MOVE    WRK-SYS-KBNCD       TO  SYS-KBNCD
           MOVE    SODR-SRYYMD         TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SODR-HOSPNUM        TO  SYS-HOSPNUM
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY)
      *****************************************************************
       900-PTNYUINRRK-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    オーダー検索処理(KEY)
      *****************************************************************
       900-ORDER-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ORDER
      *
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-ORDER
           END-IF
      *
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ORDER-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    オーダ項目検索処理(KEY)
      *****************************************************************
       900-ODRITEM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ODRITEM
      *
           MOVE    ODRITEM-REC         TO  MCPDATA-REC
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-ODRITEM
           END-IF
      *
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key"               TO MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ODRITEM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    オーダー検索処理(KEY2)
      *****************************************************************
       900-ORDER-KEY2-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-ORDER
      *
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-ORDER
           END-IF
      *
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ORDER-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理(KEY17)
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SODR-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SODR-PTID           TO  NACCT-PTID
           MOVE    SODR-SRYYMD (1:6)   TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-ZAISKBKBN
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY3)
      *****************************************************************
       900-PTNYUINRRK-KEY3-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院資料行為照会検索処理
      *****************************************************************
       900-NSRYSRH-KEY3-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-NSRYSRH
      *
           INITIALIZE                      NSRYSRH-REC
           MOVE    SPA-HOSPNUM         TO  NSRYSRH-HOSPNUM
           MOVE    SPA-PTID            TO  NSRYSRH-PTID
           MOVE    SODR-SRYYMD (1:6)   TO  NSRYSRH-SRYYM
           MOVE    "099999914"         TO  NSRYSRH-SRYCD
           MOVE    PTNYUINRRK-RRKNUM   TO  NSRYSRH-SELRRKNUM
           MOVE    SODR-SRYYMD         TO  NSRYSRH-SELSRYYMD
           MOVE    NSRYSRH-REC         TO  MCPDATA-REC
           MOVE    "tbl_nsrysrh"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NSRYSRH-REC
           ELSE
               INITIALIZE                  NSRYSRH-REC
               MOVE    1               TO  FLG-NSRYSRH
           END-IF
      *
           MOVE    "tbl_nsrysrh"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NSRYSRH-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ検索処理(KEY)
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SODR-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SODR-PTID           TO  COMB-PTID
           MOVE    WRK-COMBI-HKNCOMBI  TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
           INITIALIZE                  BUNBEN-REC
      *
           MOVE    SODR-HOSPNUM    TO  BUNBEN-HOSPNUM
           MOVE    SODR-PTID       TO  BUNBEN-PTID
           MOVE    "1"             TO  BUNBEN-NYUGAIKBN
           MOVE    TRRK-RRKNUM     TO  BUNBEN-RRKNUM
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BUNBEN-REC
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    1           TO  FLG-BUNBEN
           END-IF
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-BUNBEN-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢインサート処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＳＨＥＶＥＮＴ処理
      *****************************************************************
       910-PUSHEVENT-SEC               SECTION.
      *
           MOVE    "PUSHEVENT"         TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-PUSHEVENT-EXT.
           EXIT.
      *****************************************************************
      *    排他制御処理（チェックのみ）
      *****************************************************************
       990-LOCK-CHK-SEC                SECTION.
      *
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
      *
      *    排他制御処理
           MOVE    "I01"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SODR-PTID           TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF    ( SLOCK-RETURN    NOT =   ZERO )
               MOVE    "9999"          TO  ERRCD-ORDER
           END-IF
      *
      *    排他制御解除処理
           MOVE    ZERO                TO  SLOCK-PTID
           MOVE    3                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
      *
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
      *
           .
       990-LOCK-CHK-EXT.
           EXIT.
      *
