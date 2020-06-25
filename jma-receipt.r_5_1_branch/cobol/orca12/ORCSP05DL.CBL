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
       PROGRAM-ID.             ORCSP05DL.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者番号削除処理サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  11/03/24    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.07.00     NACL-多々納  12/11/22  患者地域連携対応
      * 04.07.00     NACL-多々納  12/12/19  患者レセプト分割対応
      * 04.07.00     NACL-多々納  13/07/XX  患者個別設定対応
      * 04.07.00     NACL-多々納  14/02/XX  患者介護情報追加対応
      * 05.00.00     NACL-多々納  16/12/15  患者個人番号
      *                                     お薬手帳管理・データ対応
      * 05.00.00     NACL-多々納  19/10/XX  削除ログ追加対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-YYK             PIC 9(01).
           03  FLG-PTSAME          PIC 9(01).
           03  FLG-OKUKAN          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PTSAME-CNT          PIC 9(02).
      *
      *R01.10  日付編集
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
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    患者番号変換マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    旧姓履歴
       01  KYUSEIRRK-REC.
           COPY    "CPKYUSEIRRK.INC".
      *
      *    低所得
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    年金
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    算定履歴
       01  SANTEIPLUS-REC.
           COPY    "CPSANTEIPLUS.INC".
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    予約マスター
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *    予約マスター保存
       01  WRK-YYK-REC.
           COPY    "CPYYK.INC"       REPLACING
                                     //YYK-// BY //WRK-YYK-//.
      *    予約メモマスター
       01  YYKCOM-REC.
           COPY    "CPYYKCOM.INC".
      *
      *    レセプトコメント
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    月代り受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者他院歴付加情報
       01  PTTAINFUKA-REC.
           COPY    "CPPTTAINFUKA.INC".
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    患者定期請求履歴
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *
      *    地方公費請求書
           COPY    "CPKOHSKY.INC".
      *
      *    患者禁忌薬剤マスタ
       01  PTKINKI-REC.
           COPY    "CPPTKINKI.INC".
      *
      *    患者特記事項マスタ
       01  PTTOKKI-REC.
           COPY    "CPPTTOKKI.INC".
      *
      *    患者公費負担額
       01  PTKOHFTNETC-REC.
           COPY    "CPPTKOHFTNETC.INC".
      *
      *    公費負担額
       01  PTKOHFTN-REC.
           COPY    "CPPTKOHFTN.INC".
      *
      *    日別収納マスタ
       01  SYUDAY-REC.
           COPY    "CPSYUDAY.INC".
      *
      *    レセプト電算マスター 
           COPY    "CPRECEDEN.INC".
      *
      *    手術歴レセプトコメントマスター
       01  OPECOM-REC.
           COPY    "CPOPECOM.INC".
      *
      *    収納合計マスタ−
       01  SYUTTL-REC.
           COPY    "CPSYUTOTAL.INC".
      *
      *    主科テーブル
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
      *
      *    療養レセプトコメントマスタ
       01  RYOCOM-REC.
           COPY    "CPRYOCOM.INC".
      *
      *    収納履歴マスタ
       01  SYURRK-REC.
           COPY    "CPSYURRK.INC".
      *
      *    患者メモ
       01  PTMEMOINF-REC.
           COPY    "CPPTMEMOINF.INC".
      *
      *    入院診療行為照会テーブル
       01  NSRYSRH-REC.
           COPY    "CPNSRYSRH.INC".
      *
      *    入院履歴照会テーブル
       01  NRRKSRH-REC.
           COPY    "CPNRRKSRH.INC".
      *
      *    分娩管理テーブル
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *    同一患者識別テーブル
       01  PTSAME-REC.
           COPY    "CPPTSAME.INC".
      *
      *    同一患者識別テーブル
       01  HZN-PTSAME-REC.
           COPY    "CPPTSAME.INC"      REPLACING
                                       //PTSAME-// BY //HZN-PTSAME-//.
      *
      *    チェック例外テーブル
           COPY    "CPCHKREIGAI.INC".
      *
      *    保険確認年月日履歴マスタ
       01  PTHKNRRK-REC.
           COPY    "CPPTHKNRRK.INC".
      *
      *    入院オーダ
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    定期請求印刷
       01  TEIKIPRT-REC.
           COPY    "CPTEIKIPRT.INC".
      *
      *    患者地域連携テーブル
       01  PTPUBLIC-REC.
           COPY    "CPPTNUM-PUBLIC.INC".
      *
      *    患者レセプト分割テーブル
       01  PT-HDIST-DAY-REC.
           COPY    "CPPT-HDIST-DAY.INC".
      *
      *    患者個別設定テーブル
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *
      *    患者介護保険情報テーブル
       01  PTCARE-HKNINF-REC.
           COPY    "CPPTCARE-HKNINF.INC".
      *
      *    患者介護認定者情報テーブル
       01  PTCARE-NINTEI-REC.
           COPY    "CPPTCARE-NINTEI.INC".
      *H28.12
      *    患者個人番号テーブル
       01  PTMYNUMBER-REC.
           COPY    "CPPTMYNUMBER.INC".
      *
      *    お薬手帳管理データ
       01  OKUSURIKANRI-REC.
           COPY    "CPOKUSURIKANRI.INC".
      *
      *    お薬手帳薬剤データ
       01  OKUSURIDATA-REC.
           COPY    "CPOKUSURIDATA.INC".
      *R01.10
      *    削除ログ
       01  PUSHINFO-REC.
           COPY    "CPPUSH-INFO.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
      *    COPY    "CPORCSDAY.INC".
      *    COPY    "CPORCSLNK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *R01.10
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  UIDIO-ST                PIC 9(2).
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  FLG-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SYORI-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SYORI-SEC
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SYORI-SEC               SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号削除処理
      *****************************************************************
       200-MAIN-SYORI-SEC               SECTION.
      *
      *    患者情報削除
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1000"              TO  SPA-ERRCD
               GO      TO    200-MAIN-SYORI-EXT
           END-IF
      *
      *    患者番号変換
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    SPA-PTID            TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    保険組合せ
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    受診履歴
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    診療科履歴
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *    患者病名
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-PTID            TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "del21"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    患者コメント
           INITIALIZE                      PTCOM-REC
           MOVE    SPA-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SPA-PTID            TO  PTCOM-PTID
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "del12"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    患者保険情報
           INITIALIZE                      PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SPA-PTID            TO  PTHKN-PTID
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    患者公費情報
           INITIALIZE                      PTKOHINF-REC
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOH-PTID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *    旧姓履歴
           INITIALIZE                      KYUSEIRRK-REC
           MOVE    SPA-HOSPNUM         TO  KYUSEI-HOSPNUM
           MOVE    SPA-PTID            TO  KYUSEI-PTID
           MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    診療会計（外来）
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    "2"                 TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    診療会計（入院）
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    "1"                 TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    診療行為（入院）
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    "1"                 TO  SRY-NYUGAIKBN
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *    診療行為（外来）
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    "2"                 TO  SRY-NYUGAIKBN
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    収納
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    収納明細
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SPA-PTID            TO  SMEI-PTID
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    収納合計
           INITIALIZE                      SYUTTL-REC
           MOVE    SPA-HOSPNUM         TO  SYUTTL-HOSPNUM
           MOVE    SPA-PTID            TO  SYUTTL-PTID
           MOVE    SYUTTL-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    低所得者
           INITIALIZE                      TSYRRK-REC
           MOVE    SPA-HOSPNUM         TO  TSYRRK-HOSPNUM
           MOVE    SPA-PTID            TO  TSYRRK-PTID
           MOVE    TSYRRK-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    受付
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-PTID            TO  UKE-PTID
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "del12"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    年金履歴
           INITIALIZE                      TNKRRK-REC
           MOVE    SPA-HOSPNUM         TO  TNKRRK-HOSPNUM
           MOVE    SPA-PTID            TO  TNKRRK-PTID
           MOVE    TNKRRK-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    算定履歴
           INITIALIZE                      SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    算定履歴付加
           INITIALIZE                      SANTEIPLUS-REC
           MOVE    SPA-HOSPNUM         TO  SANTEIPLUS-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEIPLUS-PTID
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    ワーク診療行為
           INITIALIZE                      WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "del12"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    請求管理
           INITIALIZE                      RECE10-REC
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE    SPA-PTID            TO  RECE10-PTID
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    レセプトコメント
           INITIALIZE                      RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-PTID            TO  RECECOM-PTID
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    労災保険情報
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SPA-PTID            TO  PTRSI-PTID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    月代り受給者番号
           INITIALIZE                      MONTHLYNUM-REC
           MOVE    SPA-HOSPNUM         TO  MONTHLYNUM-HOSPNUM
           MOVE    SPA-PTID            TO  MONTHLYNUM-PTID
           MOVE    MONTHLYNUM-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    入院診療行為マスター
           INITIALIZE                      NYUINACT-REC
           MOVE    SPA-HOSPNUM         TO  NSRY-HOSPNUM
           MOVE    SPA-PTID            TO  NSRY-PTID
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "del12"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    入院診療会計マスター
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    患者入院履歴
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    患者他院歴付加情報
           INITIALIZE                      PTTAINFUKA-REC
           MOVE    SPA-HOSPNUM         TO  PTTAINFUKA-HOSPNUM
           MOVE    SPA-PTID            TO  PTTAINFUKA-PTID
           MOVE    PTTAINFUKA-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_pttainfuka"    TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    患者定期請求履歴
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTTEIKIRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTTEIKIRRK-PTID
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    地方公費請求書
           INITIALIZE                      KOHSKY-REC
           MOVE    SPA-HOSPNUM         TO  KOHSKY-HOSPNUM
           MOVE    SPA-PTID            TO  KOHSKY-PTID
           MOVE    KOHSKY-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    公費負担額
           INITIALIZE                      PTKOHFTN-REC
           MOVE    SPA-HOSPNUM         TO  PTKOHFTN-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOHFTN-PTID
           MOVE    PTKOHFTN-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    日別収納マスタ（入院）
           INITIALIZE                      SYUDAY-REC
           MOVE    SPA-HOSPNUM         TO  SDAY-HOSPNUM
           MOVE    SPA-PTID            TO  SDAY-PTID
           MOVE    "1"                 TO  SDAY-NYUGAIKBN
           MOVE    SYUDAY-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syuday"        TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *    日別収納マスタ（外来）
           INITIALIZE                      SYUDAY-REC
           MOVE    SPA-HOSPNUM         TO  SDAY-HOSPNUM
           MOVE    SPA-PTID            TO  SDAY-PTID
           MOVE    "2"                 TO  SDAY-NYUGAIKBN
           MOVE    SYUDAY-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syuday"        TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    手術歴レセプトコメントマスタ
           INITIALIZE                      OPECOM-REC
           MOVE    SPA-HOSPNUM         TO  OPECOM-HOSPNUM
           MOVE    SPA-PTID            TO  OPECOM-PTID
           MOVE    OPECOM-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_opecom"        TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    レセプト電算マスタ
           INITIALIZE                      RECEDEN-REC
           MOVE    SPA-HOSPNUM         TO  RECEDEN-HOSPNUM
           MOVE    SPA-PTID            TO  RECEDEN-PTID
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_receden"       TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *    附加情報削除（入院）
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    SPA-HOSPNUM         TO  ACCTP-HOSPNUM
           MOVE    "1"                 TO  ACCTP-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCTP-PTID
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *    附加情報削除（外来）
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    SPA-HOSPNUM         TO  ACCTP-HOSPNUM
           MOVE    "2"                 TO  ACCTP-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCTP-PTID
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    主科テーブル
           INITIALIZE                  SYUKA-REC
           MOVE    SPA-HOSPNUM         TO  SYUKA-HOSPNUM
           MOVE    SPA-PTID            TO  SYUKA-PTID
      *
           MOVE    SYUKA-REC           TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syuka"         TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    療養レセプトコメント
           INITIALIZE                  RYOCOM-REC
           MOVE    SPA-HOSPNUM         TO  RYOCOM-HOSPNUM
           MOVE    SPA-PTID            TO  RYOCOM-PTID
      *
           MOVE    RYOCOM-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ryocom"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *    収納履歴マスタ
           INITIALIZE                  SYURRK-REC
           MOVE    SPA-HOSPNUM         TO  SYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  SYURRK-PTID
      *
           MOVE    SYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    患者メモ
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTMEMOINF-PTID
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    患者禁忌薬剤マスタ
           INITIALIZE                      PTKINKI-REC
           MOVE    SPA-HOSPNUM         TO  PTKINKI-HOSPNUM
           MOVE    SPA-PTID            TO  PTKINKI-PTID
           MOVE    PTKINKI-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    患者特記事項マスタ
           INITIALIZE                      PTTOKKI-REC
           MOVE    SPA-HOSPNUM         TO  PTTOKKI-HOSPNUM
           MOVE    SPA-PTID            TO  PTTOKKI-PTID
           MOVE    PTTOKKI-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_pttokki"       TO  MCP-TABLE
           MOVE    "del"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *
      *    患者レセプト分割テーブル
           INITIALIZE                      PT-HDIST-DAY-REC
           MOVE    SPA-HOSPNUM         TO  PT-HDIST-DAY-HOSPNUM
           MOVE    SPA-PTID            TO  PT-HDIST-DAY-PTID
           MOVE    PT-HDIST-DAY-REC    TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
           MOVE    "del"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    患者公費負担額マスタ
           INITIALIZE                      PTKOHFTNETC-REC
           MOVE    SPA-HOSPNUM         TO  PTKOHFTNETC-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOHFTNETC-PTID
           MOVE    PTKOHFTNETC-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    入院履歴照会テーブル
           INITIALIZE                      NRRKSRH-REC
           MOVE    SPA-HOSPNUM         TO  NRRKSRH-HOSPNUM
           MOVE    SPA-PTID            TO  NRRKSRH-PTID
           MOVE    NRRKSRH-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_nrrksrh"       TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    入院診療行為照会テーブル
           INITIALIZE                      NSRYSRH-REC
           MOVE    SPA-HOSPNUM         TO  NSRYSRH-HOSPNUM
           MOVE    SPA-PTID            TO  NSRYSRH-PTID
           MOVE    NSRYSRH-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_nsrysrh"       TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    チェック例外マスタテーブル
           INITIALIZE                      CHKREIGAI-REC
           MOVE    SPA-HOSPNUM         TO  CHKREIGAI-HOSPNUM
           MOVE    SPA-PTID            TO  CHKREIGAI-PTID
           MOVE    CHKREIGAI-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_chkreigai"     TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    分娩管理テーブル
           INITIALIZE                      BUNBEN-REC
           MOVE    SPA-HOSPNUM         TO  BUNBEN-HOSPNUM
           MOVE    SPA-PTID            TO  BUNBEN-PTID
           MOVE    BUNBEN-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_bunben"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    患者保険確認年月日履歴
           INITIALIZE                      PTHKNRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTHKNRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTHKNRRK-PTID
           MOVE    PTHKNRRK-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    入院オーダ
           INITIALIZE                      ORDER-REC
           MOVE    SPA-HOSPNUM         TO  ORDER-HOSPNUM
           MOVE    SPA-PTID            TO  ORDER-PTID
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    定期請求印刷情報
           INITIALIZE                      TEIKIPRT-REC
           MOVE    SPA-HOSPNUM         TO  TEIKIPRT-HOSPNUM
           MOVE    SPA-PTID            TO  TEIKIPRT-PTID
           MOVE    TEIKIPRT-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_teikiprt"      TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    患者地域連携
           INITIALIZE                      PTPUBLIC-REC
           MOVE    SPA-HOSPNUM         TO  PTPUBLIC-HOSPNUM
           MOVE    SPA-PTID            TO  PTPUBLIC-PTID
           MOVE    PTPUBLIC-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    患者個別設定
           INITIALIZE                      PTCONF-REC
           MOVE    SPA-HOSPNUM         TO  PTCONF-HOSPNUM
           MOVE    SPA-PTID            TO  PTCONF-PTID
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    患者介護保険情報
           INITIALIZE                      PTCARE-HKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTCRHKN-HOSPNUM
           MOVE    SPA-PTID            TO  PTCRHKN-PTID
           MOVE    PTCARE-HKNINF-REC   TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    患者介護認定者情報
           INITIALIZE                      PTCARE-NINTEI-REC
           MOVE    SPA-HOSPNUM         TO  PTCRNIN-HOSPNUM
           MOVE    SPA-PTID            TO  PTCRNIN-PTID
           MOVE    PTCARE-NINTEI-REC   TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *    患者個人番号情報
           INITIALIZE                      PTMYNUMBER-REC
           MOVE    SPA-HOSPNUM         TO  PTMYNUM-HOSPNUM
           MOVE    SPA-PTID            TO  PTMYNUM-PTID
           MOVE    PTMYNUMBER-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptmynumber"    TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *H26.5
      *    医療情報連携基盤データ(レセプト電算)
           INITIALIZE                      RECEDEN-REC
           MOVE    SPA-HOSPNUM         TO  RECEDEN-HOSPNUM
           MOVE    SPA-PTID            TO  RECEDEN-PTID
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_renkei_tmp"    TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *    ＥＦ出力データ(レセプト電算)
           INITIALIZE                      RECEDEN-REC
           MOVE    SPA-HOSPNUM         TO  RECEDEN-HOSPNUM
           MOVE    SPA-PTID            TO  RECEDEN-PTID
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_efout"         TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    予約
           PERFORM 2001-YYK-DELETE-SEC
      *
      *    同一患者識別テーブル
           PERFORM 2002-PTSAME-DELETE-SEC
      *H28.12
      *    電子お薬手帳連携テーブル
           PERFORM 2003-OKUSURIKANRI-DELETE-SEC
      *
      *R01.10
      *    削除ログ(PUSH-INFO）作成
           PERFORM 2004-PUSHINFO-INSERT-SEC
      *
      *
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約テーブル削除処理
      *****************************************************************
       2001-YYK-DELETE-SEC                SECTION.
      *
      *    予約
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-PTID            TO  YYK-PTID
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 900-YOYAKU-READ-SEC
            ELSE
                MOVE    1              TO  FLG-YYK
            END-IF
            PERFORM UNTIL    FLG-YYK   =   1
      *
      *        予約件数の減算
               MOVE    YYK-REC             TO  WRK-YYK-REC
               MOVE    ZERO                TO  YYK-YYKID
               MOVE    YYK-REC             TO  MCPDATA-REC
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "key"              TO  MCP-PATHNAME
                   PERFORM 900-YOYAKU-READ-SEC
                   COMPUTE YYK-YYKCNT          =   YYK-YYKCNT
                                               -   1
      *
                   MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
                   MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
                   MOVE    SPA-OPID            TO  YYK-OPID
      *            更新
                   MOVE    YYK-REC             TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_yyk"           TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
               END-IF
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *        削除
               MOVE    WRK-YYK-REC         TO  YYK-REC
               MOVE    YYK-REC             TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *        予約コメント
               INITIALIZE                      YYKCOM-REC
               MOVE    YYK-HOSPNUM         TO  YYKCOM-HOSPNUM
               MOVE    YYK-SRYNAIYO        TO  YYKCOM-SRYNAIYO
               MOVE    YYK-DRCD            TO  YYKCOM-DRCD
               MOVE    YYK-YYKYMD          TO  YYKCOM-YYKYMD
               MOVE    YYK-KEYYYKTIME      TO  YYKCOM-KEYYYKTIME
               MOVE    YYK-YYKID           TO  YYKCOM-YYKID
      *
               MOVE    YYKCOM-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_yykcom"        TO  MCP-TABLE
               MOVE    "del1"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 900-YOYAKU-READ-SEC
           END-PERFORM
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2001-YYK-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一患者識別テーブル削除処理
      *****************************************************************
       2002-PTSAME-DELETE-SEC                SECTION.
      *
      *    同一患者識別テーブル
           INITIALIZE                      PTSAME-REC
           MOVE    SPA-HOSPNUM         TO  PTSAME-HOSPNUM
           MOVE    SPA-PTID            TO  PTSAME-PTID
           MOVE    PTSAME-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptsame"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTSAME-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTSAME
           END-IF
           MOVE    "tbl_ptsame"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTSAME          =   ZERO
               MOVE    PTSAME-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_ptsame "       TO  MCP-TABLE
               MOVE    "del1"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *        同一患者がなくなった場合
               MOVE    PTSAME-REC          TO  MCPDATA-REC
               MOVE    "tbl_ptsame"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptsame"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-PTSAME-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-PTSAME
               END-IF
               MOVE    ZERO                TO  WRK-PTSAME-CNT
               PERFORM UNTIL    FLG-PTSAME     =   1
                   ADD     1                   TO  WRK-PTSAME-CNT
                   MOVE    PTSAME-REC          TO  HZN-PTSAME-REC
                   MOVE    "tbl_ptsame"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-PTSAME-READ-SEC
               END-PERFORM
               MOVE    "tbl_ptsame"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      WRK-PTSAME-CNT      =   1
                   MOVE    HZN-PTSAME-REC      TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_ptsame "       TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
               END-IF
           END-IF
           .
       2002-PTSAME-DELETE-EXT.
           EXIT.
      *
      *H28.12
      *
      *****************************************************************
      *    電子お薬手帳連携テーブル削除処理
      *****************************************************************
       2003-OKUSURIKANRI-DELETE-SEC    SECTION.
      *
           INITIALIZE                  OKUSURIKANRI-REC
           MOVE    SPA-HOSPNUM         TO  OKUKAN-HOSPNUM
           MOVE    SPA-PTID            TO  OKUKAN-PTID
      *
           MOVE    OKUSURIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-OKUSURIKANRI-READ-SEC
           ELSE
               INITIALIZE                  OKUSURIKANRI-REC
               MOVE    1               TO  FLG-OKUKAN
           END-IF
           PERFORM
               UNTIL   (FLG-OKUKAN     =   1   )
      *
      *        お薬手帳データ削除
               INITIALIZE                  OKUSURIDATA-REC
               MOVE    OKUKAN-DATA-KEY     TO  OKUDAT-DATA-KEY
               MOVE    OKUSURIDATA-REC     TO  MCPDATA-REC
               MOVE    "tbl_okusuri_data"  TO  MCP-TABLE
               MOVE    "del1"              TO  MCP-PATHNAME
               MOVE    "DBDELETE"          TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-OKUSURIKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    お薬手帳管理データ削除
           INITIALIZE                  OKUSURIKANRI-REC
           MOVE    SPA-HOSPNUM         TO  OKUKAN-HOSPNUM
           MOVE    SPA-PTID            TO  OKUKAN-PTID
           MOVE    OKUSURIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       2003-OKUSURIKANRI-DELETE-EXT.
           EXIT.
      *
      *R01.10
      *****************************************************************
      *    削除ログ(PUSH-INFO）作成処理
      *****************************************************************
       2004-PUSHINFO-INSERT-SEC         SECTION.
      *
      *    UIDコード取得処理
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           MOVE    SPACE               TO  PUSHINFO-REC
           INITIALIZE                      PUSHINFO-REC
           MOVE    SPA-HOSPNUM         TO  PUSH-HOSPNUM
           MOVE    C-UID               TO  PUSH-UID
      *
           MOVE    "patient_infomation"
                                       TO  PUSH-EVENT
           MOVE    "delete"            TO  PUSH-PATIENT-MODE
      *
           MOVE    SPA-PTNUM           TO  PUSH-PTNUM
           MOVE    SPA-PTID            TO  PUSH-PTID
      *    患者漢字氏名
           MOVE    SPA-NAME            TO  PUSH-PUSHINFO
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PUSH-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  PUSH-INFORMATION-TIME
      *
           MOVE    SMCNDATE-YMD        TO  PUSH-CREYMD
           MOVE    SMCNDATE-HMS        TO  PUSH-UPHMS
           MOVE    SPA-OPID            TO  PUSH-OPID
      *
           MOVE    PUSHINFO-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_push_info"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P02 PUSH_INFO INS ERR:" MCP-RC
                             ",KEY:"  PUSH-KEY
               MOVE    "9000"              TO  SPA-ERRCD
           END-IF
           .
       2004-PUSHINFO-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           INITIALIZE                  WRK-HEN-DATE
           MOVE    WRK-SYY             TO  WRK-HEN-YY
           MOVE    WRK-SMM             TO  WRK-HEN-MM
           MOVE    WRK-SDD             TO  WRK-HEN-DD
           MOVE    "-"                 TO  WRK-HEN-H1
           MOVE    "-"                 TO  WRK-HEN-H2
           IF      WRK-SYMD            =   "99999999"
               MOVE    "12"                TO  WRK-HEN-MM
               MOVE    "31"                TO  WRK-HEN-DD
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込処理
      *****************************************************************
       900-YOYAKU-READ-SEC         SECTION.
      *
           MOVE    "tbl_yyk"           TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-YYK
               MOVE    MCPDATA-REC         TO  YYK-REC
           ELSE
               MOVE    1                   TO  FLG-YYK
           END-IF
      *
           .
       900-YOYAKU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一患者識別マスター読込処理
      *****************************************************************
       900-PTSAME-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTSAME
               MOVE    MCPDATA-REC         TO  PTSAME-REC
           ELSE
               MOVE    1                   TO  FLG-PTSAME
           END-IF
      *
           .
       900-PTSAME-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    お薬手帳データ連携　管理マスター読込
      *****************************************************************
       910-OKUSURIKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  OKUSURIKANRI-REC
               MOVE    ZERO            TO  FLG-OKUKAN
           ELSE
               INITIALIZE                  OKUSURIKANRI-REC
               MOVE    1               TO  FLG-OKUKAN
           END-IF
      *
           .
       910-OKUSURIKANRI-EXT.
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
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
