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
       PROGRAM-ID.             ORCGP029.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者番号登録処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-太田     01/04/13  帳票出力時パラメータ誤り
      *  01.00.02    MCC-太田     01/05/24  パラメタの変更
      *  01.00.03    MCC-多々納   01/06/13  登録後、診療行為遷移を追加
      *  01.00.04    MCC-多々納   02/04/05  労災保険の追加
      *  01.00.05    MCC-多々納   02/06/18  月代り受給者番号の追加
      *  02.00.00    MCC-多々納   02/06/25  患者情報・旧姓履歴・患者番号
      *                                     をパラメタからＳＰＡで更新へ
      *  02.00.01    NACL-多々納  02/07/31  パラメタファイルをＤＢへ
      *  02.00.02    NACL-多々納  03/03/24  公費負担額追加
      *  02.00.03    NACL-多々納  03/04/22  保険組合変更時メッセージ追加
      *  02.00.04    NACL-多々納  03/06/27  カスタマイズ帳票名取得サブ追加
      *  02.00.05    NACL-多々納  05/11/11  MONFUNC 対応 他
      *  02.00.06    NACL-多々納  06/02/27  主科対応
      *  03.04.00    NACL-藤原    07/02/06  月途中の受給者番号変更に対応
      *  03.04.01    NACL-多々納  07/02/16  ＱＲデータ対応
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *  03.05.01    NACL-多々納  07/06/11  氏名変更の予約・受付対応
      *  04.03.00    NACL-多々納  08/07/22  特記事項追加
      *  04.05.00    NACL-多々納  09/07/XX  患者公費負担額機能追加
      *  04.07.00    NACL-多々納  11/08/XX  患者保険確認履歴追加
      *  04.07.00    NACL-多々納  12/01/12  印刷処理移動
      *  04.07.00    NACL-多々納  12/11/22  患者地域連携追加
      *  04.07.00    NACL-多々納  12/12/21  レセプト分割対応
      *  04.07.00    NACL-多々納  14/02/XX  患者介護情報追加対応
      *  04.07.00    NACL-多々納  14/03/XX  地域包括診療対象疾病対応
      *  04.08.00    NACL-多々納  15/11/XX  APIV3(HAORI)対応
      *  04.08.00    NACL-多々納  16/03/XX  平成２８年４月改正対応
      *  05.00.00    NACL-多々納  16/12/15  患者個人番号対応
      *  05.00.00    NACL-多々納  19/06/XX  難病等＋生保保険組合せ作成対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者登録全画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-KYUSEIRRK       PIC 9(01).
           03  FLG-KNJSAI          PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-YYK             PIC 9(01).
           03  FLG-TSYRRK          PIC 9(01).
           03  FLG-TNKRRK          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-MONTHLYNUM      PIC 9(01).
           03  FLG-PTKOHFTN        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PTTOKKI         PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-QRCDHKN         PIC 9(01).
           03  FLG-PTSAME          PIC 9(01).
           03  FLG-PTKOHFTNETC     PIC 9(01).
           03  FLG-PTPUBLIC        PIC 9(01).
           03  FLG-PTHDISTDAY      PIC 9(01).
           03  FLG-PTCONF          PIC 9(01).
           03  FLG-PTCARE-HKNINF   PIC 9(01).
           03  FLG-PTCARE-NINTEI   PIC 9(01).
           03  FLG-PTMYNUMBER      PIC 9(01).
      *
           03  FLG-SYORI           PIC X(01).
           03  FLG-SYORI2          PIC X(01).
           03  FLG-TSYRRK2         PIC X(01).
           03  FLG-TNKRRK2         PIC X(01).
           03  FLG-KYUSEIRRK2      PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  SYS-TIME.
               05  SYS-THH         PIC 9(02).
               05  SYS-TMM         PIC 9(02).
               05  SYS-TSS         PIC 9(02).
               05  SYS-TDD         PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDH1                PIC 9(02).
      *
           03  IDH2                PIC 9(02).
           03  IDH3                PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
      *
           03  WRK-WSYSYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-ERRCD           PIC X(04).
           03  WRK-PIDCD           PIC X(04).
      *
           03  WRK-UKEID           PIC 9(05).
           03  WRK-HKN-CNT         PIC 9(04).
           03  WRK-KOH-CNT         PIC 9(04).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-CREYMD              PIC X(08).
      *
           03  WRK-PTSAME-CNT          PIC 9(02).
      *
      *    患者個別設定　設定ＫＥＹ
           03  WRK-CONF-CKEY           PIC  X(25).
      *                  設定データ
           03  WRK-CONF-CDATA          PIC  X(10).
      *
      *    カルテＰＧ名
      *    03  WRK-KARUTE-PG           PIC X(20).
      *    処方せんＰＧ名
      *    03  WRK-SYOHOU-PG           PIC X(20).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *公費一部負担件数
       01  MAX-KOHFTN              PIC 9(03)   VALUE   300.
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    患者番号構成管理マスタ
           COPY    "CPSK1009.INC".
      *　　（帳票プログラム）
      *    COPY    "CPSK1032.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    受診内容
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    予約マスター
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *    患者番号変換（管理）
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    月代り受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *    患者公費負担
       01  PTKOHFTN-REC.
           COPY    "CPPTKOHFTN.INC".
      *
      *    患者特記事項マスタ
       01  PTTOKKI-REC.
           COPY    "CPPTTOKKI.INC".
      *
      *    患者レセプト分割テーブル
       01  PT-HDIST-DAY-REC.
           COPY    "CPPT-HDIST-DAY.INC".
      *
      *    患者公費負担額
       01  PTKOHFTNETC-REC.
           COPY    "CPPTKOHFTNETC.INC".
      *
      *    QRコード(被保険者証)
       01  QRCDHKN-REC.
           COPY    "CPQRCDHKN.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保険組合せ
       01  HZN-HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC"  REPLACING
                                     //COMB-// BY //HZN-COMB-//.
      *    保険マスタ
       01  HZN-PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC"  REPLACING
                                     //PTHKN-// BY //HZN-PTHKN-//.
      *    公費マスタ
       01  HZN-PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC"  REPLACING
                                     //PTKOH-// BY //HZN-PTKOH-//.
      *    低所得
       01  HZN-TSYRRK-REC.
           COPY    "CPTSYRRK.INC"    REPLACING
                                     //TSYRRK-// BY //HZN-TSYRRK-//.
      *
      *    年金
       01  HZN-TNKRRK-REC.
           COPY    "CPTNKRRK.INC"     REPLACING
                                     //TNKRRK-// BY //HZN-TNKRRK-//.
      *    患者労災保険情報
       01  HZN-PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC"  REPLACING
                                     //PTRSI-// BY //HZN-PTRSI-//.
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
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
      *
      *    患者地域連携テーブル
       01  PTPUBLIC-REC.
           COPY    "CPPTNUM-PUBLIC.INC".
      *
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
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
      **   COPY    "CPORCSPRTNM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    レセプト作成管理マスタ更新サブ
           COPY    "CPORCSRECEUPD.INC".
      *
      *    主情報アクセス用パラメタ
           01  ORCSSYUACCAREA.
               COPY    "CPORCSSYUACC.INC".
      *
      *    保険確認年月日履歴登録サブ
           COPY    "CPORCSPTHKNRRK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
      *
      *    初期　処理
           PERFORM 100-INIT-SEC
      *
      *    保険組合せ変更の期間外チェック
           IF     (SPA-P02-SYORI   NOT =   1    )  AND
                  (SPA-KEI2-FLG        =   ZERO )
               PERFORM 1001-HKNCOMBI-KIKANCHK-SEC
               MOVE    1               TO  FLG-END
           END-IF
      *APIV3   チェックのみ
           IF      SPA-P02-GAMEN   =   "APICHK"
               MOVE    1               TO  FLG-END
           END-IF
      *
           IF      FLG-END         =   ZERO
               IF      SPA-P02-GAMEN   =   "API"
      *            ＡＰＩ更新処理
                   PERFORM 300-API-KOUSIN-SEC
               ELSE
      *
      *            ＤＢ更新処理
                   PERFORM 200-MAIN-SEC
               END-IF
           END-IF
      *
           MOVE    SPA-P02KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
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
      *    保険組合せ変更の期間外チェック処理
      *****************************************************************
       1001-HKNCOMBI-KIKANCHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  SPA-KEI-ERRCD
           MOVE    SPACE               TO  WRK-PIDCD
      *    パラメタファイルより
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =   1 )
               OR     (SPA-KEI-ERRCD   NOT =   SPACE )
               MOVE    PARA-DATA-REC       TO  HKNCOMBI-REC
               MOVE    SPA-GMN-PTID        TO  COMB-PTID
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM 940-HKNCOMBI-READ-SEC
      *        保険変更による受診履歴修正の有無チェック
               IF      FLG-HKNCOMBI        =   ZERO
                   IF     (HZN-COMB-TEKSTYMD   NOT =   COMB-TEKSTYMD) OR
                          (HZN-COMB-TEKEDYMD   NOT =   COMB-TEKEDYMD) OR
                          (HZN-COMB-DELKBN     NOT =   COMB-DELKBN  )
                       PERFORM 2501-HKNCOMBI-CHK-SEC
                   END-IF
               END-IF
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    1                   TO  SPA-KEI2-FLG
           MOVE    SPACE               TO  SPA-KEI-ERRCD
           IF      WRK-PIDCD       NOT =   SPACE
               MOVE    WRK-PIDCD           TO  SPA-PIDCD
            END-IF
           .
       1001-HKNCOMBI-KIKANCHK-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    患者番号情報更新
           PERFORM 280-PTNUM-SYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
      *    患者情報更新
           PERFORM 210-PTINF-SYORI-SEC
      *ver.4.7
      *    患者地域連携情報更新
           PERFORM 210-PTPUBLIC-SYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *ver.4.7
      *    患者個別設定情報更新
           PERFORM 211-PTCONF-SYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
      *    旧姓履歴更新
           PERFORM 260-KYUSEIRRK-SYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
      *    公費負担額
           INITIALIZE                      PTKOHFTN-REC
           MOVE    SPA-HOSPNUM         TO  PTKOHFTN-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTKOHFTN-PTID
           MOVE    PTKOHFTN-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *    患者レセプト分割テーブル削除
           PERFORM 300-PTHDISTDAY-DELETE-SEC
      *
      *    パラメタファイルより
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL  (FLG-PARA            =   1 )
                  OR  (SPA-ERRCD       NOT =   SPACE )
               EVALUATE    PARA-FILEMEI
                   WHEN    "PTHKNINF"
                       PERFORM 220-PTHKNINF-SYORI-SEC
                       ADD     1        TO  WRK-HKN-CNT
                   WHEN    "PTKOHINF"
                       PERFORM 230-PTKOHINF-SYORI-SEC
                       ADD     1        TO  WRK-KOH-CNT
                   WHEN    "UKETUKE"
                       PERFORM 240-UKETUKE-SYORI-SEC
                   WHEN    "HKNCOMBI"
                       IF  PARA-DATA-REC   NOT =   SPACE
                           PERFORM 250-HKNCOMBI-SYORI-SEC
                       ELSE
                           CONTINUE
                       END-IF
      **********   WHEN    "KYUSEI"
      *                PERFORM 260-KYUSEIRRK-SYORI-SEC
      *            WHEN    "PTNUM"
      *************    PERFORM 280-PTNUM-SYORI-SEC
                   WHEN    "TSYRRK"
                       PERFORM 290-TSYRRK-SYORI-SEC
                   WHEN    "TNKRRK"
                       PERFORM 291-TNKRRK-SYORI-SEC
                   WHEN    "PTRSIINF"
      *                患者労災保険情報
                       PERFORM 292-PTRSIINF-SYORI-SEC
      *
                   WHEN    "PTKOHFTN"
      *                公費負担情報更新
                       PERFORM 300-PTKOHFTN-SYORI-SEC
      *H24.12
                   WHEN    "PTHDISTDAY"
      *                患者レセプト分割テーブル更新
                       PERFORM 300-PTHDISTDAY-SYORI-SEC
               END-EVALUATE
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    患者公費負担額テーブル更新
           IF      SPA-ERRCD           =   SPACE
               PERFORM 3001-PTKOHFTNETC-SYORI-SEC
           END-IF
      *    特記事項テーブル更新
           IF      SPA-ERRCD           =   SPACE
               PERFORM 300-PTTOKKI-SYORI-SEC
           END-IF
      *ver.4.7
      *    患者介護保険情報更新
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2130-PTCARE-HKNINF-SYORI-SEC
           END-IF
      *    患者介護認定者情報更新
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2140-PTCARE-NINTEI-SYORI-SEC
           END-IF
      *H28.12
      *    患者個人番号情報更新
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2150-MYNUMBER-SYORI-SEC
           END-IF
      *
      *    レセプト作成管理マスタ更新
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2009-RECEUPD-UPDATE-SEC
           END-IF
      *    主科情報設定
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-P02-SYORI       =   1     )  AND
                  (SPA-NAI-SYUKAYM NOT =   SPACE )
               PERFORM 2110-SYUKA-INSERT-SEC
           END-IF
      *    患者紐付（同一患者識別）設定
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GRPHOSPKBN      =   1     )
               PERFORM 2110-PTSAME-SYORI-SEC
           END-IF
      *
      ***  ＱＲ情報更新
      *    IF     (SPA-ERRCD           =   SPACE )  AND
      *           (SPA-QR-SYORI    NOT =   SPACE )
      *        PERFORM 2120-QRCDHKN-UPD-SEC
      **** END-IF
      *
      *ver.4.7　クライアント印刷対応の為、ORCGP02 へ移動
      *    カルテ／処方箋発行処理
      **   PERFORM 2100-HC01-SYORI-SEC
      *
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    WRK-ERRCD           TO  SPA-ERRCD
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト作成管理マスタ更新処理
      *****************************************************************
        2009-RECEUPD-UPDATE-SEC           SECTION.
      *
           INITIALIZE                      ORCSRECEUPDAREA
           MOVE    "1"                 TO  LNK-UPD-SYORI
           MOVE    SPA-HOSPNUM         TO  LNK-UPD-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  LNK-UPD-PTID
           MOVE    SPA-TERMID          TO  LNK-UPD-TERMID
           MOVE    SPA-OPID            TO  LNK-UPD-OPID
           CALL    "ORCSRECEUPD"       USING    ORCSRECEUPDAREA
                                                SPA-AREA
           IF      LNK-UPD-RC      NOT =   ZERO
               MOVE    "9000"              TO  SPA-ERRCD
               DISPLAY "LNK-UPD-RC =>" LNK-UPD-RC
           END-IF
      *
           .
        2009-RECEUPD-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    患者マスター処理
      *****************************************************************
       210-PTINF-SYORI-SEC            SECTION.
      *
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           PERFORM 900-PTINF-READ-SEC
      *
           IF      FLG-PTINF           =   1
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  PTINF-PTID
      *        入力値編集
               PERFORM 2101-PTINF-HENSYU-SEC
      *        作成年月日
               MOVE    SMCNDATE-YMD        TO  PTINF-CREYMD
      *        更新年月日
               MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
      *
               MOVE    SPA-OPID            TO  PTINF-OPID
      *
               MOVE    PTINF-REC           TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTINF INS ERR:" MCP-RC
                           ",KEY:" PTINF-KEY
                   MOVE    "9000"          TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    MCPDATA-REC         TO  PTINF-REC
      *        入力値編集
               PERFORM 2101-PTINF-HENSYU-SEC
      *        更新年月日
               MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
               MOVE    SPA-OPID            TO  PTINF-OPID
      *
               MOVE    PTINF-REC           TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTINF UPD ERR:" MCP-RC
                           ",KEY:" PTINF-KEY
                   MOVE    "9000"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       210-PTINF-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       2101-PTINF-HENSYU-SEC        SECTION.
      *
           MOVE    SPA-GMN-NAME            TO  PTINF-NAME
           MOVE    SPA-GMN-KANANAME        TO  PTINF-KANANAME
           MOVE    SPA-GMN-SEX             TO  PTINF-SEX
           IF      SPA-NAI-BIRTHDAY        =   SPACE  OR  ZERO
               MOVE    SPACE               TO  PTINF-BIRTHDAY
           ELSE
               MOVE    SPA-NAI-BIRTHDAY    TO  PTINF-BIRTHDAY
           END-IF
           MOVE    SPA-GMN-NICKNAME        TO  PTINF-NICKNAME
           MOVE    SPA-GMN-DEATHKBN        TO  PTINF-DEATHKBN
           MOVE    SPA-GMN-POST            TO  PTINF-HOME-POST
           MOVE    SPA-GMN-EDITADRS-NAME   TO  PTINF-HOME-ADRS
           MOVE    SPA-GMN-BANTI           TO  PTINF-HOME-BANTI
           MOVE    SPA-GMN-TEL1            TO  PTINF-HOME-TEL1
           MOVE    SPA-GMN-TEL2            TO  PTINF-HOME-TEL2
           MOVE    SPA-GMN-SETAINUSI       TO  PTINF-SETAINUSI
           MOVE    SPA-GMN-ZOKUGARA        TO  PTINF-ZOKUGARA
           MOVE    SPA-GMN-DISCOUNTKBN     TO  PTINF-DISCOUNT-KBN
           MOVE    SPA-GMN-DISCOUNT        TO  PTINF-DISCOUNT
           MOVE    SPA-GMN-NYUKIN-HOHO     TO  PTINF-NYUKIN-HOHO
           MOVE    SPA-GMN-SKYPRTFLG       TO  PTINF-SKYPRTFLG
           MOVE    SPA-GMN-CONDITION(1)    TO  PTINF-CONDITION-1
           MOVE    SPA-GMN-CONDITION(2)    TO  PTINF-CONDITION-2
           MOVE    SPA-GMN-CONDITION(3)    TO  PTINF-CONDITION-3
           MOVE    SPA-GMN-COMMENT-1       TO  PTINF-COMMENT-1
           MOVE    SPA-GMN-COMMENT-2       TO  PTINF-COMMENT-2
           MOVE    SPA-GMN-TABOO-1         TO  PTINF-TABOO-1
           MOVE    SPA-GMN-TABOO-2         TO  PTINF-TABOO-2
           MOVE    SPA-GMN-ALLERGY-1       TO  PTINF-ALLERGY-1
           MOVE    SPA-GMN-ALLERGY-2       TO  PTINF-ALLERGY-2
           MOVE    SPA-GMN-KANSENSYO-1     TO  PTINF-KANSENSYO-1
           MOVE    SPA-GMN-KANSENSYO-2     TO  PTINF-KANSENSYO-2
      *
      *    テスト患者番号区分
           MOVE    SPA-GMN-TSTPTNUMKBN     TO  PTINF-TSTPTNUMKBN
      *
      *    連絡先
           MOVE    SPA-GMN21-FAX            TO  PTINF-FAX
           MOVE    SPA-GMN21-KEITAI-TEL     TO  PTINF-KEITAI-TEL
           MOVE    SPA-GMN21-EMAIL          TO  PTINF-EMAIL
           MOVE    SPA-GMN21-JOB            TO  PTINF-JOB
           MOVE    SPA-GMN21-RENRAKU-NAME   TO  PTINF-RENRAKU-NAME
           MOVE    SPA-GMN21-RENRAKU-ZOKUGARA
                                            TO  PTINF-RENRAKU-ZOKUGARA
           MOVE    SPA-GMN21-RENRAKU-POST   TO  PTINF-RENRAKU-POST
           MOVE    SPA-GMN21-RENRAKU-ADRS   TO  PTINF-RENRAKU-ADRS
           MOVE    SPA-GMN21-RENRAKU-BANTI  TO  PTINF-RENRAKU-BANTI
           MOVE    SPA-GMN21-RENRAKU-TEL1   TO  PTINF-RENRAKU-TEL1
           MOVE    SPA-GMN21-RENRAKU-TEL2   TO  PTINF-RENRAKU-TEL2
           MOVE    SPA-GMN21-OFFICE-NAME    TO  PTINF-OFFICE-NAME
           MOVE    SPA-GMN21-OFFICE-POST    TO  PTINF-OFFICE-POST
           MOVE    SPA-GMN21-OFFICE-ADRS    TO  PTINF-OFFICE-ADRS
           MOVE    SPA-GMN21-OFFICE-BANTI   TO  PTINF-OFFICE-BANTI
           MOVE    SPA-GMN21-OFFICE-TEL     TO  PTINF-OFFICE-TEL
           MOVE    SPA-GMN21-KISEI-NAME     TO  PTINF-KISEI-NAME
           MOVE    SPA-GMN21-KISEI-POST     TO  PTINF-KISEI-POST
           MOVE    SPA-GMN21-KISEI-ADRS     TO  PTINF-KISEI-ADRS
           MOVE    SPA-GMN21-KISEI-BANTI    TO  PTINF-KISEI-BANTI
           MOVE    SPA-GMN21-KISEI-TEL      TO  PTINF-KISEI-TEL
      *
      *
           .
       2101-PTINF-HENSYUI-EXT.
           EXIT.
      *ver.4.7
      *****************************************************************
      *    患者地域連携情報更新処理
      *****************************************************************
       210-PTPUBLIC-SYORI-SEC        SECTION.
      *
           INITIALIZE                      PTPUBLIC-REC
           MOVE    SPA-HOSPNUM         TO  PTPUBLIC-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTPUBLIC-PTID
           MOVE    PTPUBLIC-REC        TO  MCPDATA-REC
           PERFORM 900-PTPUBLIC-READ-SEC
      *
           IF      FLG-PTPUBLIC        =   ZERO
               IF     (SPA-GMN28-PATIENTID1    =   SPACE )
                  AND (SPA-GMN28-AGREEMENT     =   SPACE )
      *            地域連携ＩＤが空白なら削除
                   MOVE    PTPUBLIC-REC        TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "P029 PTPUBLIC DEL ERR:" MCP-RC
                               ",KEY:" PTPUBLIC-KEY
                       MOVE    "9000"          TO  SPA-ERRCD
                   END-IF
               ELSE
      *            更新
                   PERFORM 2101-PTPUBLIC-UPS-SEC
               END-IF
           ELSE
               IF     (SPA-GMN28-PATIENTID1    =   SPACE )
                  AND (SPA-GMN28-AGREEMENT     =   SPACE )
                   CONTINUE
               ELSE
      *            地域連携情ＩＤ・同意区分あれば登録
                   PERFORM 2101-PTPUBLIC-INS-SEC
               END-IF
           END-IF
           .
       210-PTPUBLIC-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者地域連携データ登録処理
      *****************************************************************
       2101-PTPUBLIC-INS-SEC        SECTION.
      *
           INITIALIZE                      PTPUBLIC-REC
           MOVE    SPA-HOSPNUM         TO  PTPUBLIC-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTPUBLIC-PTID
      *    入力値編集
           MOVE    SPA-GMN28-PATIENTID1
                                       TO  PTPUBLIC-PATIENT-ID1
           MOVE    SPA-GMN28-AGREEMENT
                                       TO  PTPUBLIC-AGREEMENT
      *    作成年月日
           MOVE    SMCNDATE-YMD        TO  PTPUBLIC-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTPUBLIC-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTPUBLIC-UPHMS
           MOVE    SPA-OPID            TO  PTPUBLIC-OPID
      *
           MOVE    PTPUBLIC-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P029 PTPUBLIC INS ERR:" MCP-RC
                           ",KEY:" PTPUBLIC-KEY
               MOVE    "9000"          TO  SPA-ERRCD
           END-IF
           .
       2101-PTPUBLIC-INS-EXT.
           EXIT.
      *****************************************************************
      *    患者地域連携データ更新処理
      *****************************************************************
       2101-PTPUBLIC-UPS-SEC        SECTION.
      *
           MOVE    MCPDATA-REC         TO  PTPUBLIC-REC
      *    入力値編集
           MOVE    SPA-GMN28-PATIENTID1
                                       TO  PTPUBLIC-PATIENT-ID1
           MOVE    SPA-GMN28-AGREEMENT
                                       TO  PTPUBLIC-AGREEMENT
      *    更新年月日
           MOVE    SMCNDATE-YMD        TO  PTPUBLIC-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTPUBLIC-UPHMS
           MOVE    SPA-OPID            TO  PTPUBLIC-OPID
      *
           MOVE    PTPUBLIC-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P029 PTPUBLIC UPD ERR:" MCP-RC
                           ",KEY:" PTPUBLIC-KEY
               MOVE    "9000"          TO  SPA-ERRCD
           END-IF
           .
       2101-PTPUBLIC-UPS-EXT.
           EXIT.
      *ver.4.7
      *****************************************************************
      *    患者個別設定情報更新処理
      *****************************************************************
       211-PTCONF-SYORI-SEC        SECTION.
      *
      *    お薬手帳ＱＲ区分
           MOVE    SPA-GMN28-MNOTE-QRKBN   TO  WRK-CONF-CDATA
           MOVE    "MNOTEQR"               TO  WRK-CONF-CKEY
           PERFORM 2111-PTCONF-DBSYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      211-PTCONF-SYORI-EXT
           END-IF
      *
      *    お薬手帳データフォーマット区分
           MOVE    SPA-GMN28-MNOTE-CSV     TO  WRK-CONF-CDATA
           MOVE    "MNOTEVER"              TO  WRK-CONF-CKEY
           PERFORM 2111-PTCONF-DBSYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      211-PTCONF-SYORI-EXT
           END-IF
      *
      *    日医データフォーマット区分
           MOVE    SPA-GMN28-JMAMRVER      TO  WRK-CONF-CDATA
           MOVE    "JMAMRVER"              TO  WRK-CONF-CKEY
           PERFORM 2111-PTCONF-DBSYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      211-PTCONF-SYORI-EXT
           END-IF
      *
      *H26.1
      *    出生時体重
           IF      SPA-GMN-BIRTHWEIGHT     =   ZERO
               MOVE    SPACE                   TO  WRK-CONF-CDATA
           ELSE
               MOVE    SPA-GMN-BIRTHWEIGHT-X   TO  WRK-CONF-CDATA
           END-IF
           MOVE    "BIRTHWEIGHT"           TO  WRK-CONF-CKEY
           PERFORM 2111-PTCONF-DBSYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      211-PTCONF-SYORI-EXT
           END-IF
      *
      *H26.4
      *    地域包括診療対象疾病
           PERFORM VARYING     IDH1    FROM    1   BY  1
                   UNTIL       IDH1    >   4
               IF      SPA-GMN28-CHKHKTBYO(IDH1)  =   SPACE
                   MOVE    "0"             TO  SPA-GMN28-CHKHKTBYO(IDH1)
               END-IF
           END-PERFORM
           IF      SPA-GMN28-CHKHKTBYO-AREA    =   ZERO
               MOVE    SPACE                   TO  WRK-CONF-CDATA
           ELSE
               MOVE    SPA-GMN28-CHKHKTBYO-AREA    TO  WRK-CONF-CDATA
           END-IF
           MOVE    "TIIKIHOKATU"           TO  WRK-CONF-CKEY
           PERFORM 2111-PTCONF-DBSYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      211-PTCONF-SYORI-EXT
           END-IF
      *H28.4
      *    認知症地域包括診療
           IF      SPA-GMN28-NNCHIKBN  =   ZERO
               MOVE    SPACE           TO  SPA-GMN28-NNCHIKBN
           END-IF
           MOVE    SPA-GMN28-NNCHIKBN      TO  WRK-CONF-CDATA
           MOVE    "NINTIHOKATU"           TO  WRK-CONF-CKEY
           PERFORM 2111-PTCONF-DBSYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      211-PTCONF-SYORI-EXT
           END-IF
      *    小児かかりつけ診療
           IF      SPA-GMN28-SYOUNIKBN =   ZERO
               MOVE    SPACE           TO  SPA-GMN28-SYOUNIKBN
           END-IF
           MOVE    SPA-GMN28-SYOUNIKBN     TO  WRK-CONF-CDATA
           MOVE    "SYOUNIKAKARI"          TO  WRK-CONF-CKEY
           PERFORM 2111-PTCONF-DBSYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      211-PTCONF-SYORI-EXT
           END-IF
      *H30.4
      *    妊婦区分
           IF      SPA-GMN28-NINPUKBN   =   ZERO
               MOVE    SPACE           TO  SPA-GMN28-NINPUKBN
           END-IF
           MOVE    SPA-GMN28-NINPUKBN  TO  WRK-CONF-CDATA
           MOVE    "NINPUKBN"          TO  WRK-CONF-CKEY
           PERFORM 2111-PTCONF-DBSYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      211-PTCONF-SYORI-EXT
           END-IF
      *H28.12
      *    指導料等自動算定
           MOVE    ZERO                TO  IDH2
           PERFORM VARYING     IDH1    FROM    1   BY  1
                   UNTIL      (IDH1    >   3    )
                          OR  (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN28-AUTOSRYCD(IDH1)   =   SPACE
                   CONTINUE
               ELSE
                   ADD     1               TO  IDH2
                   MOVE    SPA-GMN28-AUTOSRYCD(IDH1)
                                           TO  WRK-CONF-CDATA
                   EVALUATE    IDH2
                   WHEN    1
                       MOVE    "JIDOSANTEI-1"      TO  WRK-CONF-CKEY
                   WHEN    2
                       MOVE    "JIDOSANTEI-2"      TO  WRK-CONF-CKEY
                   WHEN    3
                       MOVE    "JIDOSANTEI-3"      TO  WRK-CONF-CKEY
                   END-EVALUATE
                   PERFORM 2111-PTCONF-DBSYORI-SEC
               END-IF
           END-PERFORM
           IF      IDH2                <   3
               COMPUTE IDH3            =   IDH2    +   1
               PERFORM VARYING     IDH1    FROM    IDH3  BY  1
                       UNTIL      (IDH1    >   3    )
                              OR  (SPA-ERRCD   NOT =   SPACE )
                   MOVE    SPACE           TO  WRK-CONF-CDATA
                   EVALUATE    IDH1
                   WHEN    1
                       MOVE    "JIDOSANTEI-1"      TO  WRK-CONF-CKEY
                   WHEN    2
                       MOVE    "JIDOSANTEI-2"      TO  WRK-CONF-CKEY
                   WHEN    3
                       MOVE    "JIDOSANTEI-3"      TO  WRK-CONF-CKEY
                   END-EVALUATE
                   PERFORM 2111-PTCONF-DBSYORI-SEC
               END-PERFORM
           END-IF
      *
      *R01.6
      *    難病等＋生保保険組合せ作成区分
           IF      SPA-GMN27-COMBKBN   =   ZERO
               MOVE    SPACE           TO  SPA-GMN27-COMBKBN
           END-IF
           MOVE    SPA-GMN27-COMBKBN   TO  WRK-CONF-CDATA
           MOVE    "COMBKBN"           TO  WRK-CONF-CKEY
           PERFORM 2111-PTCONF-DBSYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      211-PTCONF-SYORI-EXT
           END-IF
           .
       211-PTCONF-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者個別設定登録処理
      *****************************************************************
       2111-PTCONF-DBSYORI-SEC        SECTION.
      *
      *
           INITIALIZE                  PTCONF-REC
           MOVE    PTINF-HOSPNUM       TO  PTCONF-HOSPNUM
           MOVE    PTINF-PTID          TO  PTCONF-PTID
           MOVE    WRK-CONF-CKEY       TO  PTCONF-CKEY
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptconf"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 930-PTCONF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTCONF
               INITIALIZE                  PTCONF-REC
           END-IF
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ＤＢあり
           IF      FLG-PTCONF          =   ZERO
               IF      WRK-CONF-CDATA      =   SPACE
      *            ＤＢ削除
                   MOVE    PTCONF-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_ptconf"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "P029 PTCONF DEL ERR:" MCP-RC
                               ",KEY:" PTCONF-KEY
                       MOVE    "9000"          TO  SPA-ERRCD
                   END-IF
               ELSE
      *            ＤＢ更新
                   PERFORM 21111-PTCONF-UPS-SEC
               END-IF
           ELSE
               IF      WRK-CONF-CDATA  NOT =   SPACE
      *            登録
                   PERFORM 21112-PTCONF-INS-SEC
               END-IF
           END-IF
      *
           .
       2111-PTCONF-DBSYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者個別設定データ登録処理
      *****************************************************************
       21112-PTCONF-INS-SEC        SECTION.
      *
           INITIALIZE                      PTCONF-REC
           MOVE    SPA-HOSPNUM         TO  PTCONF-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTCONF-PTID
           MOVE    WRK-CONF-CKEY       TO  PTCONF-CKEY
      *    入力値編集
           MOVE    WRK-CONF-CDATA      TO  PTCONF-CDATA
      *    作成年月日
           MOVE    SMCNDATE-YMD        TO  PTCONF-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTCONF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTCONF-UPHMS
           MOVE    SPA-OPID            TO  PTCONF-OPID
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P029 PTCONF INS ERR:" MCP-RC
                           ",KEY:" PTCONF-KEY
               MOVE    "9000"          TO  SPA-ERRCD
           END-IF
           .
       21112-PTCONF-INS-EXT.
           EXIT.
      *****************************************************************
      *    患者個別設定データ更新処理
      *****************************************************************
       21111-PTCONF-UPS-SEC        SECTION.
      *
      *    入力値編集
           MOVE    WRK-CONF-CDATA      TO  PTCONF-CDATA
      *    更新年月日
           MOVE    SMCNDATE-YMD        TO  PTCONF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTCONF-UPHMS
           MOVE    SPA-OPID            TO  PTCONF-OPID
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P029 PTCONF UPD ERR:" MCP-RC
                           ",KEY:" PTCONF-KEY
               MOVE    "9000"          TO  SPA-ERRCD
           END-IF
           .
       21111-PTCONF-UPS-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    患者保険マスター処理
      *****************************************************************
       220-PTHKNINF-SYORI-SEC        SECTION.
      *
           MOVE    PARA-DATA-REC       TO  PTHKNINF-REC
      *
           MOVE    SPA-GMN-PTID        TO  PTHKN-PTID
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           PERFORM 910-PTHKNINF-READ-SEC
      *
           IF      FLG-PTHKNINF        =   1
      *        更新情報
               MOVE    SMCNDATE-YMD        TO  PTHKN-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTHKN-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTHKN-UPHMS
      *
               MOVE    SPA-OPID            TO  PTHKN-OPID
      *
               MOVE    PTHKNINF-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
             IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTHKNINF INS ERR:" MCP-RC 
                           ",PTHKN-KEY:" PTHKN-KEY
                           ",PTHKN-TEKSTYMD:" PTHKN-TEKSTYMD
                   MOVE    "9000"          TO  SPA-ERRCD
               END-IF
           ELSE
      *        更新情報
      *D       MOVE    MCPDATA-REC         TO  HZN-PTHKNINF-REC
               MOVE    HZN-PTHKN-CREYMD    TO  PTHKN-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTHKN-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTHKN-UPHMS
      *
               MOVE    SPA-OPID            TO  PTHKN-OPID
      *
               MOVE    PTHKNINF-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTHKNINF UPD ERR:" MCP-RC
                           ",PTHKN-KEY:" PTHKN-KEY
                           ",PTHKN-TEKSTYMD:" PTHKN-TEKSTYMD
                   MOVE    "9000"          TO  SPA-ERRCD
               END-IF
      *        確認年月日履歴
               IF     (SPA-ERRCD           =   SPACE )
                  AND (HZN-PTHKN-KAKUNINYMD    NOT =   SPACE )
                  AND (PTHKN-KAKUNINYMD        NOT =   SPACE )
                   IF      HZN-PTHKN-KAKUNINYMD    =   PTHKN-KAKUNINYMD
                       CONTINUE
                   ELSE
      *                今回変更あり
                       PERFORM 2209-PTHKNRRK-ADD-SEC
                   END-IF
               END-IF
           END-IF
           .
       220-PTHKNINF-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者保険確認年月日履歴処理
      *****************************************************************
       2209-PTHKNRRK-ADD-SEC        SECTION.
      *
           INITIALIZE                      ORCSPTHKNRRKAREA
           MOVE    SPA-GMN-PTID        TO  ORCSHKNRRK-PTID
           MOVE    PTHKN-HKNID         TO  ORCSHKNRRK-HKNID
           MOVE    HZN-PTHKN-KAKUNINYMD
                                       TO  ORCSHKNRRK-KAKUNINYMD
           CALL    "ORCSPTHKNRRK"      USING
                                       SPA-AREA
                                       ORCSPTHKNRRKAREA
           IF      ORCSHKNRRK-RC       =   9
               DISPLAY "P029 PTHKNRRK UPD ERR:" ORCSHKNRRK-RC
                       ",PTHKN-HKNID:" PTHKN-HKNID
               MOVE    "9000"          TO  SPA-ERRCD
           END-IF
           .
       2209-PTHKNRRK-ADD-EXT.
           EXIT.
      *****************************************************************
      *    患者公費マスター処理
      *****************************************************************
       230-PTKOHINF-SYORI-SEC        SECTION.
      *
           MOVE    PARA-DATA-REC        TO  PTKOHINF-REC
      *
           MOVE    SPA-GMN-PTID        TO  PTKOH-PTID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           PERFORM 920-PTKOHINF-READ-SEC
      *
           IF      FLG-PTKOHINF        =   1
               MOVE    SPA-GMN-PTID        TO  PTKOH-PTID
      *        更新情報
               MOVE    SMCNDATE-YMD        TO  PTKOH-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTKOH-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTKOH-UPHMS
      *
               MOVE    SPA-OPID            TO  PTKOH-OPID
      *
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTKOHINF INS ERR:" MCP-RC
                           ",KEY:" PTKOH-KEY
                   MOVE    "9000"          TO  SPA-ERRCD
               END-IF
           ELSE
      *        更新情報
      *D       MOVE    MCPDATA-REC         TO  HZN-PTKOHINF-REC
               MOVE    HZN-PTKOH-CREYMD    TO  PTKOH-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTKOH-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTKOH-UPHMS
      *
               MOVE    SPA-OPID            TO  PTKOH-OPID
      *
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTKOHINF UPD ERR:" MCP-RC
                           ",KEY:" PTKOH-KEY
                   MOVE    "9000"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    月代り受給者番号登録（生活保護で、新規登録の時）
           IF      PTKOH-KOHNUM        =   "012"
      *        残留邦人追加
                                       OR  "025"
               PERFORM 2301-MONTHLYNUM-SYORI-SEC
           END-IF
           .
       230-PTKOHINF-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    月代り受給者番号マスター処理
      *****************************************************************
       2301-MONTHLYNUM-SYORI-SEC        SECTION.
      *
           MOVE    PARA-DATA-REC       TO  PTKOHINF-REC
           MOVE    SPA-GMN-PTID        TO  PTKOH-PTID
      *
           IF      PTKOH-SAKJOKBN      =   "1"
      *        削除分
               MOVE    SPACE               TO  MONTHLYNUM-REC
               INITIALIZE                      MONTHLYNUM-REC
               MOVE    PTKOH-HOSPNUM       TO  MONTHLYNUM-HOSPNUM
               MOVE    PTKOH-PTID          TO  MONTHLYNUM-PTID
               MOVE    PTKOH-KOHNUM        TO  MONTHLYNUM-KOHNUM
               MOVE    "2"                 TO  MONTHLYNUM-NYUGAIKBN
      *        適用開始年月日
               MOVE    PTKOH-TEKSTYMD(1:6) TO  MONTHLYNUM-SRYYM
               MOVE    "tbl_monthlynum"    TO  WRK-MCP-TABLE
               MOVE    "key"               TO  WRK-MCP-PATHNAME
               PERFORM 925-MONTHLYNUM-READ-SEC
               IF      FLG-MONTHLYNUM      =   ZERO
                   MOVE    MONTHLYNUM-REC      TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_monthlynum"    TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
               END-IF
           ELSE
      *        編集
               MOVE    SPACE               TO  MONTHLYNUM-REC
               INITIALIZE                      MONTHLYNUM-REC
               MOVE    PTKOH-HOSPNUM       TO  MONTHLYNUM-HOSPNUM
               MOVE    PTKOH-PTID          TO  MONTHLYNUM-PTID
               MOVE    PTKOH-KOHID         TO  MONTHLYNUM-KOHID
               MOVE    "2"                 TO  MONTHLYNUM-NYUGAIKBN
      *        適用開始年月日
               MOVE    PTKOH-TEKSTYMD(1:6) TO  MONTHLYNUM-SRYYM
               MOVE    "tbl_monthlynum"    TO  WRK-MCP-TABLE
               MOVE    "key4"              TO  WRK-MCP-PATHNAME
               PERFORM 925-MONTHLYNUM-READ-SEC
               IF      FLG-MONTHLYNUM      =   ZERO
      *            修正分
                   IF     (PTKOH-JKYSNUM   NOT =   SPACE )  AND
                          (PTKOH-JKYSNUM   NOT =   MONTHLYNUM-JKYSNUM)
                       MOVE    PTKOH-JKYSNUM   TO  MONTHLYNUM-JKYSNUM
      *                更新情報
                       MOVE    SMCNDATE-YMD        TO  MONTHLYNUM-UPYMD
                       MOVE    SMCNDATE-HMS        TO  MONTHLYNUM-UPHMS
      *
                       MOVE    SPA-OPID            TO  MONTHLYNUM-OPID
      *
                       MOVE    MONTHLYNUM-REC      TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_monthlynum"    TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       IF      MCP-RC          NOT =   ZERO
                            DISPLAY "P029 MONTHLYNUM UPD ERR:" MCP-RC
                                   ",KEY:" MONTHLYNUM-KEY
                           MOVE    "9000"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               ELSE
      **           MOVE    SPACE               TO  MONTHLYNUM-REC
      *            INITIALIZE                      MONTHLYNUM-REC
      *            MOVE    PTKOH-HOSPNUM       TO  MONTHLYNUM-HOSPNUM
      *            MOVE    PTKOH-PTID          TO  MONTHLYNUM-PTID
      *            MOVE    PTKOH-KOHNUM        TO  MONTHLYNUM-KOHNUM
      *            MOVE    "2"                 TO  MONTHLYNUM-NYUGAIKBN
      *            適用開始年月日
      *            MOVE    PTKOH-TEKSTYMD(1:6) TO  MONTHLYNUM-SRYYM
      *            MOVE    "tbl_monthlynum"    TO  WRK-MCP-TABLE
      *            MOVE    "key"               TO  WRK-MCP-PATHNAME
      *            PERFORM 925-MONTHLYNUM-READ-SEC
      *            IF      FLG-MONTHLYNUM      =   ZERO
      *                修正分
      *                IF     (PTKOH-JKYSNUM   NOT =   SPACE )  AND
      *                       (PTKOH-JKYSNUM   NOT =
      *                                            MONTHLYNUM-JKYSNUM)
      *                    MOVE    PTKOH-JKYSNUM   TO MONTHLYNUM-JKYSNUM
      *                    更新情報
      *                    MOVE    SMCNDATE-YMD    TO  MONTHLYNUM-UPYMD
      *                    MOVE    SMCNDATE-HMS    TO  MONTHLYNUM-UPHMS
      *
      *                    MOVE    SPA-OPID        TO  MONTHLYNUM-OPID
      *
      *                    MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
      *                    MOVE    "DBUPDATE"      TO  MCP-FUNC
      *                    MOVE    "tbl_monthlynum"
      *                                            TO  MCP-TABLE
      *                    MOVE    "key"           TO  MCP-PATHNAME
grpsys*                    CALL    "ORCDBMAIN"     USING   MCPAREA
      *                                                    MCPDATA-REC
      *                                                    SPA-AREA
      *                    IF      MCP-RC          NOT =   ZERO
      *                       DISPLAY "P029 MONTHLYNUM UPD ERR:" MCP-RC
      *                              ",KEY:" MONTHLYNUM-KEY
      *                      MOVE    "9000"          TO  SPA-ERRCD
      *                    END-IF
      *                END-IF
      *********    ELSE
                       IF      PTKOH-JKYSNUM       =   SPACE
                           GO      TO      2301-MONTHLYNUM-SYORI-EXT
                       END-IF
      *                新規
                       MOVE    SPACE           TO  MONTHLYNUM-REC
                       INITIALIZE                  MONTHLYNUM-REC
                       MOVE    PTKOH-HOSPNUM   TO  MONTHLYNUM-HOSPNUM
                       MOVE    PTKOH-PTID      TO  MONTHLYNUM-PTID
                       MOVE    PTKOH-KOHNUM    TO  MONTHLYNUM-KOHNUM
                       MOVE    PTKOH-KOHID     TO  MONTHLYNUM-KOHID
                       MOVE    PTKOH-TEKSTYMD(1:6)
                                               TO  MONTHLYNUM-SRYYM
                       MOVE    "2"             TO  MONTHLYNUM-NYUGAIKBN
                       MOVE    PTKOH-JKYSNUM   TO  MONTHLYNUM-JKYSNUM
      *
                       MOVE    SMCNDATE-YMD    TO  MONTHLYNUM-CREYMD
                       MOVE    SMCNDATE-YMD    TO  MONTHLYNUM-UPYMD
                       MOVE    SMCNDATE-HMS    TO  MONTHLYNUM-UPHMS
      *
                       MOVE    SPA-OPID        TO  MONTHLYNUM-OPID
      *
                       MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                       MOVE    "DBINSERT"      TO  MCP-FUNC
                       MOVE    "tbl_monthlynum"
                                               TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       IF      MCP-RC          NOT =   ZERO
                           DISPLAY "P029 MONTHLYNUM INS ERR:" MCP-RC
                                       ",KEY:" MONTHLYNUM-KEY
                           MOVE    "9000"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      ******** END-IF    
           END-IF
      *
      *
           .
       2301-MONTHLYNUM-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    受診受付マスター処理
      *****************************************************************
       240-UKETUKE-SYORI-SEC        SECTION.
      *
           MOVE    PARA-DATA-REC        TO  UKETUKE-REC
      *
      *    削除以外の診療科、ドクターを診療行為入力の為ＳＰＡへセット
           IF     (UKE-UPYMD       NOT =   ALL "9")  AND
                  (UKE-KAIKEITIME      =   ZERO   )
               MOVE    UKE-SRYKA           TO  SPA-SRYKA
               MOVE    UKE-DRCD            TO  SPA-DRCD
           END-IF
      *
           MOVE    SPA-GMN-PTID        TO  UKE-PTID
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           PERFORM 930-UKETUKE-READ-SEC
      *
           IF      FLG-UKETUKE         =   1
      *        追加の時、受付ＩＤを取得
               MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
               MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
               MOVE    ZERO                TO  UKE-UKEID
               MOVE    UKETUKE-REC         TO  MCPDATA-REC
               PERFORM 930-UKETUKE-READ-SEC
               IF      FLG-UKETUKE         =   ZERO
                   ADD     1                   TO  UKE-PTID
                   IF      UKE-PTID            >   99999
                      MOVE    "9001"              TO  SPA-ERRCD
                      GO      TO      240-UKETUKE-SYORI-EXT
                   END-IF
                   MOVE    UKE-PTID            TO  WRK-UKEID
      *
                   MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
                   MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
                   MOVE    SPA-OPID            TO  UKE-OPID
      *
                   MOVE    UKETUKE-REC         TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_uketuke"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "P029 UKETUKE UPD ERR:" MCP-RC 
                               ",KEY:"  UKE-KEY
                       MOVE    "9002"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   INITIALIZE                  UKETUKE-REC
                   MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
                   MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
                   MOVE    ZERO                TO  UKE-UKEID
                   MOVE    1                   TO  UKE-PTID
                   MOVE    UKE-PTID            TO  WRK-UKEID
      *
                   MOVE    SMCNDATE-YMD        TO  UKE-CREYMD
                   MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
                   MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
                   MOVE    SPA-OPID            TO  UKE-OPID
      *
                   MOVE    UKETUKE-REC         TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "tbl_uketuke"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "P029 UKETUKE INS ERR:" MCP-RC 
                               ",KEY:"  UKE-KEY
                       MOVE    "9002"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               MOVE    PARA-DATA-REC       TO  UKETUKE-REC
               MOVE    SPA-GMN-PTID        TO  UKE-PTID
               MOVE    WRK-UKEID           TO  UKE-UKEID
      *
               MOVE    SMCNDATE-YMD        TO  UKE-CREYMD
               MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
               MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
               MOVE    SPA-OPID            TO  UKE-OPID
      *
               MOVE    UKETUKE-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 UKETUKE INS ERR:" MCP-RC
                               ",KEY:"  UKE-KEY
                   MOVE    "9002"          TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    PARA-DATA-REC       TO  UKETUKE-REC
               MOVE    SPA-GMN-PTID        TO  UKE-PTID
      *
               IF      UKE-UPYMD       =   ALL "9"
      *            削除
                   MOVE    UKETUKE-REC         TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_uketuke"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "P029 UKETUKE UPD ERR:" MCP-RC
                               ",KEY:"  UKE-KEY
                   END-IF
               ELSE
      *
                   MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
                   MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
                   MOVE    SPA-OPID            TO  UKE-OPID
      *
      *            更新
                   MOVE    UKETUKE-REC         TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_uketuke"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "P029 UKETUKE UPD ERR:" MCP-RC
                               ",KEY:"  UKE-KEY
                       MOVE    "9002"          TO  SPA-ERRCD
                   END-IF
                END-IF
           END-IF
      *
      *    予約者の来院済みを処理
           IF      UKE-KEYYYKTIME  NOT =   SPACE
               PERFORM 2401-YYK-SYORI-SEC
           END-IF
           .
       240-UKETUKE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約処理
      *****************************************************************
       2401-YYK-SYORI-SEC             SECTION.
      *
           INITIALIZE                  YYK-REC
           MOVE    UKE-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    UKE-SRYNAIYO        TO  YYK-SRYNAIYO
           MOVE    UKE-DRCD            TO  YYK-DRCD
           MOVE    UKE-UKEYMD          TO  YYK-YYKYMD
           MOVE    UKE-KEYYYKTIME      TO  YYK-KEYYYKTIME
           MOVE    UKE-YYKID           TO  YYK-YYKID
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-YYK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-YYK
           END-IF
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *H26.8   ドクター・診療内容を変更した場合
           IF      FLG-YYK             =   ZERO
               IF     (YYK-PTID        NOT =   ZERO )
                AND   (YYK-PTID        NOT =   UKE-PTID)
                   MOVE    1               TO  FLG-YYK
               END-IF
           END-IF
      *
           IF      FLG-YYK             =   ZERO
      *
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *
               MOVE    SPA-OPID            TO  YYK-OPID
      *
               IF      UKE-UPYMD       =   ALL "9"
                   IF      YYK-RAIINZUMI   =   "1"
                       MOVE    SPACE               TO  YYK-RAIINZUMI
                       MOVE    YYK-REC             TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_yyk"           TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                   END-IF
               ELSE
                   IF     (YYK-RAIINZUMI       =   SPACE   )
                       OR (YYK-PTID            =   ZERO    )
                       OR (YYK-NAME        NOT =   SPA-GMN-NAME )
                       MOVE    "1"                 TO  YYK-RAIINZUMI
      *                患者ＩＤ
                       IF     (YYK-PTID            =   ZERO    )
                           MOVE    SPA-GMN-PTID        TO  YYK-PTID
                       END-IF
      *                氏名変更
                       MOVE    SPA-GMN-NAME        TO  YYK-NAME
      *
                       MOVE    YYK-REC             TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_yyk"           TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                   END-IF
               END-IF
           END-IF
      *
           .
       2401-YYK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ処理
      *****************************************************************
       250-HKNCOMBI-SYORI-SEC        SECTION.
      *
           MOVE    PARA-DATA-REC       TO  HKNCOMBI-REC
      *    非表示区分設定
           PERFORM 2502-HKNCOMBI-HYOJI-SEC
      *
           MOVE    SPA-GMN-PTID        TO  COMB-PTID
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           PERFORM 940-HKNCOMBI-READ-SEC
      *    保険変更による受診履歴修正の有無チェック
           IF      FLG-HKNCOMBI        =   ZERO
      *D       MOVE    MCPDATA-REC         TO  HZN-HKNCOMBI-REC
               IF     (HZN-COMB-TEKSTYMD   NOT =   COMB-TEKSTYMD) OR
                      (HZN-COMB-TEKEDYMD   NOT =   COMB-TEKEDYMD) OR
                      (HZN-COMB-DELKBN     NOT =   COMB-DELKBN  )
                   PERFORM 2501-HKNCOMBI-CHK-SEC
               END-IF
           END-IF
      *    新規設定を変更区分クリア
           IF      COMB-CHGKBN         =   "A"
               MOVE    SPACE               TO  COMB-CHGKBN
           END-IF
      *
           IF      FLG-HKNCOMBI        =   1
      *
               MOVE    SMCNDATE-YMD        TO  COMB-CREYMD
               MOVE    SMCNDATE-YMD        TO  COMB-UPYMD
               MOVE    SMCNDATE-HMS        TO  COMB-UPHMS
      *
               MOVE    SPA-OPID            TO  COMB-OPID
      *
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 HKNCOMBI INS ERR:" MCP-RC
                           ",KEY:" COMB-KEY
                   MOVE    "9003"          TO  SPA-ERRCD
               END-IF
           ELSE
      *
               MOVE    HZN-COMB-CREYMD     TO  COMB-CREYMD
               MOVE    SMCNDATE-YMD        TO  COMB-UPYMD
               MOVE    SMCNDATE-HMS        TO  COMB-UPHMS
      *
               MOVE    SPA-OPID            TO  COMB-OPID
      *
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 HKNCOMBI UPD ERR:" MCP-RC
                           ",KEY:" COMB-KEY
                   MOVE    "9003"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       250-HKNCOMBI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険変更による受診履歴修正の有無チェック処理
      *****************************************************************
       2502-HKNCOMBI-HYOJI-SEC             SECTION.
      *
           PERFORM VARYING     IDX5    FROM    1   BY  1
                   UNTIL      (IDX5        >   SPA-HYO-MAX )
               IF     (COMB-HKNCOMBINUM
                                       =   SPA-HYO-HKNCOMBINUM(IDX5))
               AND    (COMB-HKNNUM     =   SPA-HYO-HKNNUM     (IDX5))
               AND    (COMB-KOH1HKNNUM =   SPA-HYO-KOH1HKNNUM (IDX5))
               AND    (COMB-KOH2HKNNUM =   SPA-HYO-KOH2HKNNUM (IDX5))
               AND    (COMB-KOH3HKNNUM =   SPA-HYO-KOH3HKNNUM (IDX5))
               AND    (COMB-KOH4HKNNUM =   SPA-HYO-KOH4HKNNUM (IDX5))
                   MOVE    SPA-HYO-HYOJIKBN (IDX5)
                                           TO  COMB-HYOJIKBN 
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
          .
       2502-HKNCOMBI-HYOJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険変更による受診履歴修正の有無チェック処理
      *****************************************************************
       2501-HKNCOMBI-CHK-SEC             SECTION.
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-REC
           MOVE    HZN-COMB-HOSPNUM    TO  JYURRK-HOSPNUM
           MOVE    HZN-COMB-PTID       TO  JYURRK-PTID
           MOVE    HZN-COMB-HKNCOMBINUM
                                       TO  JYURRK-HKNCOMBINUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key41"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                  JYURRK-REC
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-JYURRK          =   ZERO
               IF      COMB-DELKBN         =   SPACE
                   IF     (JYURRK-SRYYMD       <   COMB-TEKSTYMD)  OR
                          (JYURRK-SRYYMD       >   COMB-TEKEDYMD)
                       MOVE    "9100"          TO  SPA-KEI-ERRCD
                       MOVE    "K910"          TO  WRK-PIDCD
                   END-IF
               ELSE
      *            削除の時
                   MOVE    "9100"          TO  SPA-KEI-ERRCD
                   MOVE    "K910"          TO  WRK-PIDCD
               END-IF
           END-IF
      *
      *    病名チェック
           MOVE    SPACE               TO  PTBYOMEI-REC
           INITIALIZE                  PTBYOMEI-REC
           MOVE    HZN-COMB-HOSPNUM    TO  PTBYO-HOSPNUM
           MOVE    HZN-COMB-PTID       TO  PTBYO-PTID
           MOVE    HZN-COMB-HKNCOMBINUM
                                       TO  PTBYO-HKNCOMBI
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 980-PTBYOMEI-READ-SEC
           ELSE
               MOVE    SPACE               TO  PTBYOMEI-REC
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTBYOMEI        =   ZERO
               IF      COMB-DELKBN         =   SPACE
                   IF     (PTBYO-SRYYMD        <   COMB-TEKSTYMD)  OR
                          (PTBYO-SRYYMD        >   COMB-TEKEDYMD)
                       IF      SPA-KEI-ERRCD       =   SPACE
                           MOVE    "9101"          TO  SPA-KEI-ERRCD
                           MOVE    "K911"          TO  WRK-PIDCD
                       ELSE
                           MOVE    "9102"          TO  SPA-KEI-ERRCD
                           MOVE    "K912"          TO  WRK-PIDCD
                       END-IF
                   END-IF
               ELSE
      *            削除の時
                   IF      SPA-KEI-ERRCD       =   SPACE
                       MOVE    "9101"          TO  SPA-KEI-ERRCD
                       MOVE    "K911"          TO  WRK-PIDCD
                   ELSE
                       MOVE    "9102"          TO  SPA-KEI-ERRCD
                       MOVE    "K912"          TO  WRK-PIDCD
                   END-IF
               END-IF
           END-IF
      *
           .
       2501-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧姓履歴マスター処理
      *****************************************************************
       260-KYUSEIRRK-SYORI-SEC             SECTION.
      *
      *    旧姓履歴はすべて削除
           PERFORM 260-KYUSEIRRK-DEL-SEC
      *
      *
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-NAI25-KYU-MAX
               IF     SPA-GMN25-KYU-DELKBN (IDX)   =   SPACE
                   INITIALIZE                      KYUSEIRRK-REC
                   MOVE    SPA-HOSPNUM         TO  KYUSEI-HOSPNUM
                   MOVE    SPA-GMN-PTID        TO  KYUSEI-PTID
                   MOVE    SPA-GMN25-KYU-CHGYMD (IDX)
                                               TO  KYUSEI-CHGYMD
                   MOVE    SPA-GMN25-KYU-KANANAME (IDX)
                                               TO  KYUSEI-KANANAME
                   MOVE    SPA-GMN25-KYU-NAME  (IDX)
                                               TO  KYUSEI-NAME
                   MOVE    SPA-GMN25-KYU-NICKNAME (IDX)
                                               TO  KYUSEI-NICKNAME
      ************ MOVE    SPA-GMN25-KYU-UPYMD (IDX)
      *                                        TO  KYUSEI-UPYMD
      *            MOVE    SPA-GMN25-KYU-CREYMD (IDX)
      *************                            TO  KYUSEI-CREYMD
      *
      *            更新日が当日以外は更新とする
                   IF      SPA-GMN25-KYU-UPYMD (IDX)
                                               =   SMCNDATE-YMD
                       MOVE    SMCNDATE-YMD        TO  KYUSEI-CREYMD
                       MOVE    SMCNDATE-YMD        TO  KYUSEI-UPYMD
                       MOVE    SMCNDATE-HMS        TO  KYUSEI-UPHMS
                   ELSE
                       MOVE    SMCNDATE-YMD        TO  KYUSEI-UPYMD
                       MOVE    SMCNDATE-HMS        TO  KYUSEI-UPHMS
                   END-IF
      *
                   MOVE    SPA-OPID            TO  KYUSEI-OPID
      *
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
                   MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "P029 KYUSEIRRK INS ERR:" MCP-RC
                               ",KEY:" KYUSEI-KEY
                       MOVE    "9000"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       260-KYUSEIRRK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧姓履歴マスター処理
      *****************************************************************
       260-KYUSEIRRK-DEL-SEC            SECTION.
      *
           INITIALIZE                      KYUSEIRRK-REC
           MOVE    SPA-HOSPNUM         TO  KYUSEI-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  KYUSEI-PTID
           MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       260-KYUSEIRRK-DEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号変換（管理）マスター処理
      *****************************************************************
       280-PTNUM-SYORI-SEC             SECTION.
      *
      *    患者ＩＤ決定
           IF      SPA-GMN-PTID        =   ZERO
      *        患者番号採番検索
               INITIALIZE                  SYS-1009-REC
               MOVE    "1009"              TO  SYS-1009-KANRICD
               MOVE    "*"                 TO  SYS-1009-KBNCD
               MOVE    SPA-SYSYMD          TO  SYS-1009-STYUKYMD
               MOVE    SPA-SYSYMD          TO  SYS-1009-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
               MOVE    SYS-1009-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 960-SYSKANRI-READ-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC         TO  SYS-1009-REC
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      FLG-SYSKANRI        =   1
                   MOVE    "1011"          TO  SPA-ERRCD
                   DISPLAY "P29 SYS1009(PTGETNUM)ERR:" MCP-RC  "."
                   GO      TO      280-PTNUM-SYORI-EXT
               END-IF
               IF      SYS-1009-PTID       =   ALL "9"
                   MOVE    "8001"              TO  SPA-ERRCD
                   MOVE    ZERO                TO  SYS-1009-PTID
               END-IF
               COMPUTE SYS-1009-PTID           =   SYS-1009-PTID  +   1
      *        ＰＴＩＤ＝ゼロ　はなし
               IF      SYS-1009-PTID           =   ZERO
                   MOVE    "8001"              TO  SPA-ERRCD
                   MOVE    1                   TO  SYS-1009-PTID
               END-IF
               MOVE    SYS-1009-PTID           TO  SPA-GMN-PTID
      *
               MOVE    SMCNDATE-YMD        TO  SYS-1009-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1009-UPHMS
               MOVE    SPA-OPID            TO  SYS-1009-OPID
      *        患者番号採番  更新
               MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
               MOVE    SYS-1009-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC         NOT  =   ZERO
                   MOVE    "1011"          TO  SPA-ERRCD
                   DISPLAY "P012 SYSKANRI UPDATE:" MCP-RC "."
                   GO      TO      280-PTNUM-SYORI-EXT
               END-IF
           END-IF
      *
      *
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTNUM-PTID
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 950-PTNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNUM
           END-IF
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTNUM           =   1
               INITIALIZE                      PTNUM-REC
               MOVE    SPA-HOSPNUM             TO  PTNUM-HOSPNUM
               MOVE    SPA-GMN-PTID            TO  PTNUM-PTID
               MOVE    SPA-GMN-PTNUM           TO  PTNUM-PTNUM
               MOVE    SPA-PTNUM-HKNID         TO  PTNUM-HKNID
               MOVE    SPA-PTNUM-KOHID         TO  PTNUM-KOHID
               MOVE    SPA-PTNUM-AUTOCOMBINUM  TO  PTNUM-AUTOCOMBINUM
               MOVE    SPA-PTNUM-MANUCOMBINUM  TO  PTNUM-MANUCOMBINUM
      *
               MOVE    SMCNDATE-YMD        TO  PTNUM-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTNUM-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTNUM-UPHMS
      *
               MOVE    SPA-OPID            TO  PTNUM-OPID
      *
               MOVE    PTNUM-REC           TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTNUM INS ERR:" MCP-RC
                          ",KEY:" PTNUM-KEY   "."
                   MOVE    "9000"          TO  SPA-ERRCD
               END-IF
           ELSE
               IF      SPA-GMN-PTNUM       NOT =   PTNUM-PTNUM
                   MOVE    "8003"              TO  SPA-ERRCD
                   GO      TO      280-PTNUM-SYORI-EXT
               END-IF
               MOVE    SPA-HOSPNUM             TO  PTNUM-HOSPNUM
               MOVE    SPA-GMN-PTID            TO  PTNUM-PTID
               MOVE    SPA-GMN-PTNUM           TO  PTNUM-PTNUM
               MOVE    SPA-PTNUM-HKNID         TO  PTNUM-HKNID
               MOVE    SPA-PTNUM-KOHID         TO  PTNUM-KOHID
               MOVE    SPA-PTNUM-AUTOCOMBINUM  TO  PTNUM-AUTOCOMBINUM
               MOVE    SPA-PTNUM-MANUCOMBINUM  TO  PTNUM-MANUCOMBINUM
      *
               MOVE    SMCNDATE-YMD        TO  PTNUM-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTNUM-UPHMS
      *
               MOVE    SPA-OPID            TO  PTNUM-OPID
      *
               MOVE    PTNUM-REC           TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTNUM UPD ERR:" MCP-RC "."
                          ",KEY:" PTNUM-KEY   "."
                   MOVE    "9000"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       280-PTNUM-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    低所得マスター処理
      *****************************************************************
       290-TSYRRK-SYORI-SEC            SECTION.
      *
           MOVE    PARA-DATA-REC       TO  TSYRRK-REC
           MOVE    SPA-GMN-PTID        TO  TSYRRK-PTID
      *
           EVALUATE    TRUE
      *    削除
           WHEN    ( TSYRRK-UPYMD      =   ALL "9" )
               MOVE    TSYRRK-REC      TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC      NOT =   ZERO
                   DISPLAY "P029 TSYRRK DEL ERR:" MCP-RC
                          ",KEY:" TSYRRK-KEY
               END-IF
      *    変更
           WHEN    ( TSYRRK-NINSTYMD-O     NOT =   SPACE )
      *
      *DD      MOVE    MCPDATA-REC         TO  HZN-TSYRRK-REC
      *DD      MOVE    HZN-TSYRRK-CREYMD   TO  TSYRRK-CREYMD
               MOVE    SMCNDATE-YMD        TO  TSYRRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  TSYRRK-UPHMS
      *
               MOVE    SPA-OPID            TO  TSYRRK-OPID
      *
               MOVE    TSYRRK-REC      TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC      NOT =   ZERO
                   DISPLAY "P029 TSYRRK UPD ERR:" MCP-RC
                          ",KEY:" TSYRRK-KEY
                   MOVE    "9000"          TO  SPA-ERRCD
               END-IF
      *    追加
           WHEN    OTHER
               MOVE    SPA-GMN-PTID    TO  TSYRRK-PTID
      *
               MOVE    SMCNDATE-YMD        TO  TSYRRK-CREYMD
               MOVE    SMCNDATE-YMD        TO  TSYRRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  TSYRRK-UPHMS
      *
               MOVE    SPA-OPID            TO  TSYRRK-OPID
      *
               MOVE    TSYRRK-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 TSYRRK INS ERR:" MCP-RC
                          ",KEY:" TSYRRK-KEY
                   MOVE    "9000"          TO  SPA-ERRCD
               END-IF
           END-EVALUATE
      *
           .
       290-TSYRRK-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    年金マスター処理
      *****************************************************************
       291-TNKRRK-SYORI-SEC             SECTION.
      *
           MOVE    PARA-DATA-REC       TO  TNKRRK-REC
           MOVE    SPA-GMN-PTID        TO  TNKRRK-PTID
      *
           EVALUATE    TRUE
      *    削除
           WHEN    ( TNKRRK-UPYMD      =   ALL "9" )
               MOVE    TNKRRK-REC      TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC      NOT =   ZERO
                   DISPLAY "P029 TNKRRK DEL ERR:" MCP-RC
                          ",KEY:" TNKRRK-KEY
               END-IF
      *    変更
           WHEN    ( TNKRRK-STYMD-O    NOT =   SPACE )
      *
      *DD      MOVE    MCPDATA-REC         TO  HZN-TNKRRK-REC
      *DD      MOVE    HZN-TNKRRK-CREYMD   TO  TNKRRK-CREYMD
               MOVE    SMCNDATE-YMD        TO  TNKRRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNKRRK-UPHMS
      *
               MOVE    SPA-OPID            TO  TNKRRK-OPID
      *
               MOVE    TNKRRK-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC      NOT =   ZERO
                   DISPLAY "P029 TNKRRK UPD ERR:" MCP-RC
                          ",KEY:" TNKRRK-KEY
                   MOVE    "9000"          TO  SPA-ERRCD
               END-IF
      *    追加
           WHEN    OTHER
               MOVE    SPA-GMN-PTID        TO  TNKRRK-PTID
      *
               MOVE    SMCNDATE-YMD        TO  TNKRRK-CREYMD
               MOVE    SMCNDATE-YMD        TO  TNKRRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNKRRK-UPHMS
      *
               MOVE    SPA-OPID            TO  TNKRRK-OPID
      *
               MOVE    TNKRRK-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 TNKRRK INS ERR:" MCP-RC
                          ",KEY:" TNKRRK-KEY
                   MOVE    "9000"          TO  SPA-ERRCD
               END-IF
           END-EVALUATE
           .
       291-TNKRRK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報マスター処理
      *****************************************************************
       292-PTRSIINF-SYORI-SEC        SECTION.
      *
           MOVE    PARA-DATA-REC       TO  PTRSIINF-REC
      *
           MOVE    SPA-GMN-PTID        TO  PTRSI-PTID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           PERFORM 990-PTRSIINF-READ-SEC
      *
           IF      FLG-PTRSIINF        =   1
      *
               MOVE    SMCNDATE-YMD        TO  PTRSI-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTRSI-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTRSI-UPHMS
      *
               MOVE    SPA-OPID            TO  PTRSI-OPID
      *
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTRSIINF INS ERR:" MCP-RC
                          ",KEY:" PTRSI-KEY
                   MOVE    "9000"          TO  SPA-ERRCD
               END-IF
           ELSE
      *
               MOVE    HZN-PTRSI-CREYMD    TO  PTRSI-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTRSI-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTRSI-UPHMS
      *
               MOVE    SPA-OPID            TO  PTRSI-OPID
      *
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTRSIINF UPD ERR:" MCP-RC
                          ",KEY:" PTRSI-KEY
                   MOVE    "9000"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       292-PTRSIINF-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担情報更新処理
      *****************************************************************
       300-PTKOHFTN-SYORI-SEC         SECTION.
      *
           MOVE    PARA-DATA-REC       TO  PTKOHFTN-REC
      *    公費負担読込み
           MOVE    SPA-GMN-PTID        TO  PTKOHFTN-PTID
           MOVE    PTKOHFTN-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 991-PTKOHFTN-READ-SEC
           ELSE 
               MOVE    1               TO  FLG-PTKOHFTN
           END-IF
           MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTKOHFTN        =   1
               MOVE    PARA-DATA-REC       TO  PTKOHFTN-REC
      *
               MOVE    SMCNDATE-YMD        TO  PTKOHFTN-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTKOHFTN-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTKOHFTN-UPHMS
      *
               MOVE    SPA-OPID            TO  PTKOHFTN-OPID
               MOVE    SPACE               TO  PTKOHFTN-TERMID
      *
      *        新規
               MOVE    SPA-GMN-PTID        TO  PTKOHFTN-PTID
               MOVE    PTKOHFTN-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTKOHFTN INS ERR:" MCP-RC
                                   ",KEY:" PTKOHFTN-KEY
                   MOVE    "9000"              TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    PTKOHFTN-CREYMD     TO  WRK-CREYMD
               MOVE    PARA-DATA-REC       TO  PTKOHFTN-REC
      *
               MOVE    WRK-CREYMD          TO  PTKOHFTN-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTKOHFTN-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTKOHFTN-UPHMS
      *
               MOVE    SPA-OPID            TO  PTKOHFTN-OPID
      *
      *        更新
               MOVE    PTKOHFTN-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTKOHFTN UPD ERR:" MCP-RC
                                   ",KEY:" PTKOHFTN-KEY
                   MOVE    "9000"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       300-PTKOHFTN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    特記事項情報更新処理
      *****************************************************************
       300-PTTOKKI-SYORI-SEC         SECTION.
      *
      *    特記事項はすべて削除
           INITIALIZE                      PTTOKKI-REC
           MOVE    SPA-HOSPNUM         TO  PTTOKKI-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTTOKKI-PTID
           MOVE    PTTOKKI-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_pttokki"       TO  MCP-TABLE
           MOVE    "del"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-GMN26-TMAX
               INITIALIZE                  PTTOKKI-REC
               MOVE    SPA-HOSPNUM         TO  PTTOKKI-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  PTTOKKI-PTID
               MOVE    SPA-NAI26-TNYUGIKBN(IDX)
                                           TO  PTTOKKI-NYUGAIKBN
               MOVE    IDX                 TO  PTTOKKI-RENNUM
      *        特記事項
               MOVE    SPA-GMN26-TCD(IDX)
                                           TO  PTTOKKI-CD
               MOVE    SPA-GMN26-TNAIYO(IDX)
                                           TO  PTTOKKI-NAIYO
               MOVE    SPA-NAI26-TSTYM (IDX)
                                           TO  PTTOKKI-STYM
               MOVE    SPA-NAI26-TEDYM (IDX)
                                           TO  PTTOKKI-EDYM
      *
               MOVE    SMCNDATE-YMD        TO  PTTOKKI-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTTOKKI-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTTOKKI-UPHMS
               MOVE    SPA-OPID            TO  PTTOKKI-OPID
      *    追加
               MOVE    PTTOKKI-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_pttokki"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTTOKKI INS ERR:" MCP-RC
                                   ",KEY:" PTTOKKI-KEY
                   MOVE    "9000"              TO  SPA-ERRCD
               END-IF
           END-PERFORM
           .
       300-PTTOKKI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者レセプト分割削除処理
      *****************************************************************
       300-PTHDISTDAY-DELETE-SEC         SECTION.
      *
      *    患者レセプト分割はすべて削除
           INITIALIZE                      PT-HDIST-DAY-REC
           MOVE    SPA-HOSPNUM         TO  PT-HDIST-DAY-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PT-HDIST-DAY-PTID
           MOVE    PT-HDIST-DAY-REC    TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
           MOVE    "del"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       300-PTHDISTDAY-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    患者レセプト分割更新処理
      *****************************************************************
       300-PTHDISTDAY-SYORI-SEC         SECTION.
      *
           MOVE    PARA-DATA-REC       TO  PT-HDIST-DAY-REC
      *
           MOVE    SPA-HOSPNUM         TO  PT-HDIST-DAY-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PT-HDIST-DAY-PTID
      *
           MOVE    PT-HDIST-DAY-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 991-PTHDISTDAY-READ-SEC
           ELSE 
               MOVE    1               TO  FLG-PTHDISTDAY
           END-IF
           MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTHDISTDAY        =   1
               MOVE    PARA-DATA-REC       TO  PT-HDIST-DAY-REC
               MOVE    SPA-HOSPNUM         TO  PT-HDIST-DAY-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  PT-HDIST-DAY-PTID
      *
               MOVE    SMCNDATE-YMD        TO  PT-HDIST-DAY-CREYMD
               MOVE    SMCNDATE-YMD        TO  PT-HDIST-DAY-UPYMD
               MOVE    SMCNDATE-HMS        TO  PT-HDIST-DAY-UPHMS
      *
               MOVE    SPA-OPID            TO  PT-HDIST-DAY-OPID
      *
      *        新規
               MOVE    SPA-GMN-PTID        TO  PT-HDIST-DAY-PTID
               MOVE    PT-HDIST-DAY-REC    TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PT-HDIST-DAY INS ERR:" MCP-RC
                                   ",KEY:" PT-HDIST-DAY-KEY
                   MOVE    "9000"              TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    PT-HDIST-DAY-CREYMD     TO  WRK-CREYMD
               MOVE    PARA-DATA-REC       TO  PT-HDIST-DAY-REC
      *
               MOVE    WRK-CREYMD          TO  PT-HDIST-DAY-CREYMD
               MOVE    SMCNDATE-YMD        TO  PT-HDIST-DAY-UPYMD
               MOVE    SMCNDATE-HMS        TO  PT-HDIST-DAY-UPHMS
      *
               MOVE    SPA-OPID            TO  PT-HDIST-DAY-OPID
      *
      *        更新
               MOVE    PT-HDIST-DAY-REC    TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PT-HDIST-DAY UPD ERR:" MCP-RC
                                   ",KEY:" PT-HDIST-DAY-KEY
                   MOVE    "9000"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       300-PTHDISTDAY-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者公費負担額テーブル更新処理
      *****************************************************************
       3001-PTKOHFTNETC-SYORI-SEC         SECTION.
      *
      *    今回更新対象月分はすべて削除
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL      (IDX     >   MAX-KOHFTN )  OR
                              (SPA-NAI24-KOHFTN-ID (IDX)   =   ZERO)
               INITIALIZE                      PTKOHFTNETC-REC
               MOVE    SPA-HOSPNUM         TO  PTKOHFTNETC-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  PTKOHFTNETC-PTID
               MOVE    SPA-NAI24-KOHFTN-ID (IDX)
                                           TO  PTKOHFTNETC-KOHID
               MOVE    SPA-NAI24-KOHFTN-YM (IDX)
                                           TO  PTKOHFTNETC-SRYYMD(1:6)
               MOVE    "%"                 TO  PTKOHFTNETC-SRYYMD(7:1)
               MOVE    PTKOHFTNETC-REC     TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
               IF      SPA-NAI24-KOHFTN-YM (IDX)   =   SPACE
      *            公費削除
                   MOVE    "del2"              TO  MCP-PATHNAME
               ELSE
                   MOVE    "del3"              TO  MCP-PATHNAME
               END-IF
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-PERFORM
      *    
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHFTNETC"       TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL   FLG-PARA            =   1
      *
               PERFORM 30011-PTKOHFTNETC-DBSYORI-SEC
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3001-PTKOHFTNETC-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    公費負担情報更新処理
      *****************************************************************
       30011-PTKOHFTNETC-DBSYORI-SEC         SECTION.
      *
           MOVE    PARA-DATA-REC       TO  PTKOHFTNETC-REC
      *    公費負担読込み
           MOVE    SPA-GMN-PTID        TO  PTKOHFTNETC-PTID
           MOVE    PTKOHFTNETC-REC     TO  MCPDATA-REC
           MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 991-PTKOHFTNETC-READ-SEC
           ELSE 
               MOVE    1               TO  FLG-PTKOHFTN
           END-IF
           MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTKOHFTNETC     =   1
               MOVE    PARA-DATA-REC       TO  PTKOHFTNETC-REC
      *
               MOVE    SMCNDATE-YMD        TO  PTKOHFTNETC-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTKOHFTNETC-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTKOHFTNETC-UPHMS
      *
               MOVE    SPA-OPID            TO  PTKOHFTNETC-OPID
      *
      *        新規
               MOVE    SPA-GMN-PTID        TO  PTKOHFTNETC-PTID
               MOVE    PTKOHFTNETC-REC     TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTKOHFTNETC INS ERR:" MCP-RC
                                   ",KEY:" PTKOHFTNETC-KEY
                   MOVE    "9000"              TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    PTKOHFTNETC-CREYMD  TO  WRK-CREYMD
               MOVE    PARA-DATA-REC       TO  PTKOHFTNETC-REC
      *
               MOVE    WRK-CREYMD          TO  PTKOHFTNETC-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTKOHFTNETC-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTKOHFTNETC-UPHMS
      *
               MOVE    SPA-OPID            TO  PTKOHFTNETC-OPID
      *
      *        更新
               MOVE    PTKOHFTNETC-REC     TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTKOHFTNETC UPD ERR:" MCP-RC
                                   ",KEY:" PTKOHFTN-KEY
                   MOVE    "9000"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       30011-PTKOHFTNETC-DBSYORI-EXT.
           EXIT.
      *ver.4.7
      *****************************************************************
      *    患者介護保険情報更新処理
      *****************************************************************
       2130-PTCARE-HKNINF-SYORI-SEC         SECTION.
      *
      *    すべて削除
           INITIALIZE                      PTCARE-HKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTCRHKN-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTCRHKN-PTID
           MOVE    PTCARE-HKNINF-REC   TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL      (IDX     >   SPA-GMN28-CRHKN-MAX )
                           OR (SPA-ERRCD   NOT =   SPACE )
      *        登録
               PERFORM 21301-PTCAREHKNINF-INS-SEC
           END-PERFORM
           .
       2130-PTCARE-HKNINF-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者介護保険情報データ登録処理
      *****************************************************************
       21301-PTCAREHKNINF-INS-SEC        SECTION.
      *
           INITIALIZE                  PTCARE-HKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTCRHKN-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTCRHKN-PTID
           MOVE    SPA-GMN28-TCRHKNJANUM (IDX)
                                       TO  PTCRHKN-HKNJANUM
           MOVE    SPA-GMN28-TCRHIHKNJANUM (IDX)
                                       TO  PTCRHKN-HIHKNJANUM
           MOVE    SPA-NAI28-TCRHSTRYMD(IDX)
                                       TO  PTCRHKN-TEKSTYMD
           MOVE    SPA-NAI28-TCRHENDYMD(IDX)
                                       TO  PTCRHKN-TEKEDYMD
      *    作成年月日
           MOVE    SMCNDATE-YMD        TO  PTCRHKN-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTCRHKN-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTCRHKN-UPHMS
           MOVE    SPA-OPID            TO  PTCRHKN-OPID
      *
           MOVE    PTCARE-HKNINF-REC   TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptcare_hkninf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P029 PTCRHKN INS ERR:" MCP-RC
                           ",KEY:" PTCRHKN-KEY
               MOVE    "9000"          TO  SPA-ERRCD
           END-IF
           .
       21301-PTCAREHKNINF-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者介護認定者情報更新処理
      *****************************************************************
       2140-PTCARE-NINTEI-SYORI-SEC         SECTION.
      *
      *    すべて削除
           INITIALIZE                      PTCARE-NINTEI-REC
           MOVE    SPA-HOSPNUM         TO  PTCRNIN-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTCRNIN-PTID
           MOVE    PTCARE-NINTEI-REC   TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-GMN28-CRNIN-MAX
               INITIALIZE                  PTCARE-NINTEI-REC
               MOVE    SPA-HOSPNUM         TO  PTCRNIN-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  PTCRNIN-PTID
               MOVE    SPA-NAI28-TCRSTRYMD(IDX)
                                           TO  PTCRNIN-TEKSTYMD
               MOVE    SPA-NAI28-TCRENDYMD(IDX)
                                           TO  PTCRNIN-TEKEDYMD
               MOVE    SPA-NAI28-TCAREJOTAI(IDX)
                                           TO  PTCRNIN-CAREJOTAICD
               MOVE    SPA-NAI28-TCRNINYMD (IDX)
                                           TO  PTCRNIN-NINTEIYMD
      *
               MOVE    SMCNDATE-YMD        TO  PTCRNIN-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTCRNIN-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTCRNIN-UPHMS
               MOVE    SPA-OPID            TO  PTCRNIN-OPID
      *        追加
               MOVE    PTCARE-NINTEI-REC   TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTCARE-NINTEI INS ERR:" MCP-RC
                                   ",KEY:" PTCRNIN-KEY
                   MOVE    "9000"              TO  SPA-ERRCD
               END-IF
           END-PERFORM
           .
       2140-PTCARE-NINTEI-SYORI-EXT.
           EXIT.
      *
      *H28.12
      *****************************************************************
      *    患者個人番号情報更新処理
      *****************************************************************
       2150-MYNUMBER-SYORI-SEC         SECTION.
      *
      *    すべて削除
           INITIALIZE                      PTMYNUMBER-REC
           MOVE    SPA-HOSPNUM         TO  PTMYNUM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTMYNUM-PTID
           MOVE    PTMYNUMBER-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptmynumber"    TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-GMN28-MYID-MAX
               INITIALIZE                  PTMYNUMBER-REC
               MOVE    SPA-HOSPNUM         TO  PTMYNUM-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  PTMYNUM-PTID
               MOVE    SPA-GMN28-TMYIDKEY(IDX)
                                           TO  PTMYNUM-ID-KEY
               MOVE    SPA-GMN28-TMYNUMBER(IDX)
                                           TO  PTMYNUM-MY-NUMBER
               MOVE    SPA-GMN28-TMYDESCRIP(IDX)
                                           TO  PTMYNUM-DESCRIPTION
      *
               MOVE    SMCNDATE-YMD        TO  PTMYNUM-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTMYNUM-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTMYNUM-UPHMS
               MOVE    SPA-OPID            TO  PTMYNUM-OPID
      *        追加
               MOVE    PTMYNUMBER-REC      TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_ptmynumber"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTMYNUMBER INS ERR:" MCP-RC
                                   ",KEY:" PTMYNUM-KEY
                   MOVE    "9000"              TO  SPA-ERRCD
               END-IF
           END-PERFORM
           .
       2150-MYNUMBER-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    主科情報設定処理
      *****************************************************************
       2110-SYUKA-INSERT-SEC         SECTION.
      *
           INITIALIZE                      ORCSSYUACCAREA
      *    処理区分
           MOVE    "02"                TO  LNK-SYUACC-I-KBN
      *    診療年月
           MOVE    SPA-NAI-SYUKAYM     TO  LNK-SYUACC-I-SRYYM
           MOVE    SPA-NAI-SYUKA       TO  LNK-SYUACC-IO-H-SRYKA
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           CALL    "ORCSSYUKAMAIN"     USING
                                       SPA-AREA
                                       ORCSSYUACCAREA
           IF       LNK-SYUACC-RC  NOT =   ZERO
               DISPLAY "P029 ORCSSYUKAMAIN INS ERR:" LNK-SYUACC-RC
               MOVE    "9000"              TO  SPA-ERRCD
           END-IF
      *
           .
       2110-SYUKA-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    患者紐付（同一患者識別）更新処理
      *****************************************************************
       2110-PTSAME-SYORI-SEC         SECTION.
      *
           INITIALIZE                      PTSAME-REC
           MOVE    SPA-HOSPNUM         TO  PTSAME-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTSAME-PTID
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
           IF      FLG-PTSAME          =   1
               IF      SPA-NAI-GRPHOSPNUM  NOT =   ZERO
      *            同一患者識別追加
                   PERFORM 21101-PTSAME-ADD-SEC
               END-IF
           ELSE
               IF      SPA-NAI-GRPHOSPNUM      =   ZERO
      *            同一識別削除
                   PERFORM 21102-PTSAME-DELETE-SEC
               ELSE
                   IF     (SPA-NAI-GRPHOSPNUM  =   PTSAME-SAME-HOSPNUM)
                      AND (SPA-NAI-GRPPTID     =   PTSAME-SAME-PTID   )
                       CONTINUE
                   ELSE
      *            紐付け変更
      *                同一識別削除
                       PERFORM 21102-PTSAME-DELETE-SEC
      *                同一患者識別追加
                       PERFORM 21101-PTSAME-ADD-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       2110-PTSAME-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一患者識別追加処理
      *****************************************************************
       21101-PTSAME-ADD-SEC         SECTION.
      *
           INITIALIZE                      PTSAME-REC
           MOVE    SPA-NAI-GRPHOSPNUM  TO  PTSAME-HOSPNUM
           MOVE    SPA-NAI-GRPPTID     TO  PTSAME-PTID
           MOVE    PTSAME-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptsame"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTSAME-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTSAME
           END-IF
           MOVE    "tbl_ptsame"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PTSAME           =   ZERO
      *        既に同一患者識別が登録済み
               MOVE    SPA-HOSPNUM         TO  PTSAME-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  PTSAME-PTID
               PERFORM 211011-PTSAME-INS-SEC
           ELSE
      *        同一患者識別なし
               INITIALIZE                      PTSAME-REC
               MOVE    SPA-NAI-GRPHOSPNUM  TO  PTSAME-HOSPNUM
               MOVE    SPA-NAI-GRPPTID     TO  PTSAME-PTID
               MOVE    SPA-NAI-GRPHOSPNUM  TO  PTSAME-SAME-HOSPNUM
               MOVE    SPA-NAI-GRPPTID     TO  PTSAME-SAME-PTID
               PERFORM 211011-PTSAME-INS-SEC
      *
               INITIALIZE                      PTSAME-REC
               MOVE    SPA-HOSPNUM         TO  PTSAME-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  PTSAME-PTID
               MOVE    SPA-NAI-GRPHOSPNUM  TO  PTSAME-SAME-HOSPNUM
               MOVE    SPA-NAI-GRPPTID     TO  PTSAME-SAME-PTID
               PERFORM 211011-PTSAME-INS-SEC
           END-IF
           .
       21101-PTSAME-ADD-EXT.
           EXIT.
      *****************************************************************
      *    同一患者識別追加処理
      *****************************************************************
       211011-PTSAME-INS-SEC         SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  PTSAME-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTSAME-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTSAME-UPHMS
      *
           MOVE    SPA-OPID            TO  PTSAME-OPID
      *
           MOVE    PTSAME-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptsame"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P029 PTSAME INS ERR:" MCP-RC
                          ",KEY:" PTSAME-KEY
               MOVE    "9000"          TO  SPA-ERRCD
           END-IF
           .
       211011-PTSAME-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *   同一識別削除加処理
      *****************************************************************
       21102-PTSAME-DELETE-SEC         SECTION.
      *
           MOVE    PTSAME-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptsame "       TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *    同一患者がなくなった場合
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
      *
           .
       21102-PTSAME-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲ情報更新処理
      *****************************************************************
       2120-QRCDHKN-UPD-SEC         SECTION.
      *
           INITIALIZE                      QRCDHKN-REC
           MOVE    SPA-HOSPNUM         TO  QRCDHKN-HOSPNUM
           MOVE    SPA-QR-REGISTYMD    TO  QRCDHKN-REGISTYMD
           MOVE    SPA-QR-REGISTID     TO  QRCDHKN-REGISTID
           MOVE    QRCDHKN-REC         TO  MCPDATA-REC
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-QRCDHKN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-QRCDHKN
               INITIALIZE                      QRCDHKN-REC
           END-IF
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-QRCDHKN         =   ZERO
               MOVE    SPA-GMN-PTID        TO  QRCDHKN-PTID
               MOVE    1                   TO  QRCDHKN-MOD-FLG
               MOVE    SMCNDATE-YMD        TO  QRCDHKN-UPYMD
               MOVE    SMCNDATE-HMS        TO  QRCDHKN-UPHMS
      *
               MOVE    SPA-OPID            TO  QRCDHKN-OPID
      *
               MOVE    QRCDHKN-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "9000"              TO  SPA-ERRCD
                   DISPLAY "P029 QRCDHKN KEY UPD ERR:"  MCP-RC
                       ",KEY:" QRCDHKN-KEY
               END-IF
           END-IF
      *
           .
       2120-QRCDHKN-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ患者保険情報更新処理
      *****************************************************************
       300-API-KOUSIN-SEC         SECTION.
      *
      *    保険・公費・保険組合せのみ
      *    パラメタファイルより
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL  (FLG-PARA            =   1 )
               EVALUATE    PARA-FILEMEI
                   WHEN    "PTHKNINF"
                       PERFORM 220-PTHKNINF-SYORI-SEC
                       ADD     1        TO  WRK-HKN-CNT
                   WHEN    "PTKOHINF"
                       PERFORM 230-PTKOHINF-SYORI-SEC
                       ADD     1        TO  WRK-KOH-CNT
                   WHEN    "HKNCOMBI"
                       PERFORM 250-HKNCOMBI-SYORI-SEC
      *            労災保険
                   WHEN    "PTRSIINF"
                       IF      SPA-RSIAPI-FLG      =   "1"
                           PERFORM 292-PTRSIINF-SYORI-SEC
                       END-IF
               END-EVALUATE
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       300-API-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-PTINF
                   MOVE    MCPDATA-REC     TO  PTINF-REC
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HZN-PTHKNINF-REC
                   MOVE    ZERO            TO  FLG-PTHKNINF
               ELSE
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費マスター読み込み
      *****************************************************************
       920-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HZN-PTKOHINF-REC
                   MOVE    ZERO            TO  FLG-PTKOHINF
               ELSE
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       920-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費マスター読み込み
      *****************************************************************
       925-MONTHLYNUM-READ-SEC         SECTION.
      *
           MOVE    MONTHLYNUM-REC      TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-MONTHLYNUM
                   MOVE    MCPDATA-REC     TO  MONTHLYNUM-REC
               ELSE
                   MOVE    1               TO  FLG-MONTHLYNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-MONTHLYNUM
           END-IF
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       925-MONTHLYNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険マスター読み込み
      *****************************************************************
       990-PTRSIINF-READ-SEC         SECTION.
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HZN-PTRSIINF-REC
                   MOVE    ZERO                TO  FLG-PTRSIINF
               ELSE
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       990-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付マスター読み込み
      *****************************************************************
       930-UKETUKE-READ-SEC         SECTION.
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  UKETUKE-REC
                   MOVE    ZERO                TO  FLG-UKETUKE
               ELSE
                   MOVE    1               TO  FLG-UKETUKE
               END-IF
           ELSE
               MOVE    1               TO  FLG-UKETUKE
           END-IF
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       930-UKETUKE-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者番号変換読み込み
      *****************************************************************
       950-PTNUM-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNUM-REC
               MOVE    ZERO            TO  FLG-PTNUM
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           .
       950-PTNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    予約読み込み
      *****************************************************************
       980-YYK-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  YYK-REC
               MOVE    ZERO                TO  FLG-YYK
           ELSE
               MOVE    1                   TO  FLG-YYK
           END-IF
      *
           .
       980-YYK-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       940-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HZN-HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   INITIALIZE                  HZN-HKNCOMBI-REC
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    公費負担マスター読み込み
      *****************************************************************
       991-PTKOHFTN-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHFTN-REC
               MOVE    ZERO            TO  FLG-PTKOHFTN
           ELSE
               INITIALIZE                  PTKOHFTN-REC
               MOVE    1               TO  FLG-PTKOHFTN
           END-IF
      *
           .
       991-PTKOHFTN-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者公費負担額マスター読み込み
      *****************************************************************
       991-PTKOHFTNETC-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHFTNETC-REC
               MOVE    ZERO            TO  FLG-PTKOHFTNETC
           ELSE
               INITIALIZE                  PTKOHFTNETC-REC
               MOVE    1               TO  FLG-PTKOHFTNETC
           END-IF
      *
           .
       991-PTKOHFTNETC-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       980-PTBYOMEI-READ-SEC         SECTION.
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
       980-PTBYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-SYSKANRI-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
      ******   MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       960-SYSKANRI-READ-EXT.
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
      *    QRコード(被保険者証)読込
      *****************************************************************
       900-QRCDHKN-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  QRCDHKN-REC
               MOVE    ZERO            TO  FLG-QRCDHKN
           ELSE
               INITIALIZE                  QRCDHKN-REC
               MOVE    1               TO  FLG-QRCDHKN
           END-IF
           .
       900-QRCDHKN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者地域連携読み込み
      *****************************************************************
       900-PTPUBLIC-READ-SEC         SECTION.
      *
           MOVE    PTPUBLIC-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTPUBLIC-REC
                   MOVE    ZERO            TO  FLG-PTPUBLIC
               ELSE
                   MOVE    1               TO  FLG-PTPUBLIC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTPUBLIC
           END-IF
           MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTPUBLIC-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者レセプト分割テーブル読み込み
      *****************************************************************
       991-PTHDISTDAY-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PT-HDIST-DAY-REC
               MOVE    ZERO            TO  FLG-PTHDISTDAY
           ELSE
               INITIALIZE                  PT-HDIST-DAY-REC
               MOVE    1               TO  FLG-PTHDISTDAY
           END-IF
      *
           .
       991-PTHDISTDAY-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者個別設定テーブル読み込み
      *****************************************************************
       930-PTCONF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTCONF-REC
               MOVE    ZERO            TO  FLG-PTCONF
           ELSE
               INITIALIZE                  PTCONF-REC
               MOVE    1               TO  FLG-PTCONF
           END-IF
      *
           .
       930-PTCONF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者介護保険情読み込み
      *****************************************************************
       900-PTCAREHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTCARE-HKNINF-REC   TO  MCPDATA-REC
           MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTCARE-HKNINF-REC
                   MOVE    ZERO            TO  FLG-PTCARE-HKNINF
               ELSE
                   MOVE    1               TO  FLG-PTCARE-HKNINF
                   MOVE    SPACE           TO  PTCARE-HKNINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTCARE-HKNINF
           END-IF
           MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTCAREHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *
