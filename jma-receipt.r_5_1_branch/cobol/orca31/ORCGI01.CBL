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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGI01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院業務
      *  コンポーネント名  : 入退院登録（Ｉ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02.08.09    ひさなが 　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    02.12.10  収納明細削除処理追加
      *  01.00.02    NACL-太田    02.12.26  入院科の初期表示を修正
      *  01.00.03    NACL-太田    03.01.24  食事の集計方法修正
      *  01.00.04    NACL-太田    03.02.03  退院時診療行為チェック追加
      *  01.00.05    NACL-太田    03.02.05  自院歴作成データを考慮する
      *  01.00.06    NACL-太田    03.03.05  異動取消機能を追加
      *                                     入院歴の表示を修正
      *  01.00.07    NACL-太田    03.03.06  通算日数を関数より取得する
      *                                     よう修正
      *  01.00.08    NACL-太田    03.03.07  排他制御対応
      *  01.00.09    NACL-太田    03.03.12  選定療養費１８０日越えの
      *                                     日数算定方法修正
      *  01.00.10    NACL-太田    03.03.12  カルテ印刷機能追加
      *  01.00.11    NACL-太田    03.03.14  転科・転棟・転室時、入院料
      *                                     のみの変更も可とする
      *  01.00.12    NACL-太田    03.03.18  入院カスタマイズ帳票対応
      *  01.00.13    NACL-太田    03.04.02  負担金計算サブ引数修正
      *  01.00.14    NACL-太田    03.04.08  患者登録への遷移を追加
      *  01.00.15    NACL-太田    03.04.09  保険組合せの取得方法修正
      *  01.00.16    NACL-太田    03.05.08  入院歴表示の自院歴退院日の
      *                                     編集を修正（入院中対応）
      *  01.00.17    NACL-太田    03.05.16  入院カルテに保険を編集する
      *  01.00.18    NACL-太田    03.05.19  退院時診療行為チェック追加
      *  01.00.19    NACL-太田    03.06.05  入院歴の修正を日付項目以外は
      *                                     いつでも変更可とする
      *  01.00.20    NACL-小豆沢  03.06.23  診療所老人医療管理料の転室、
      *                                     退院取消、異動取消の正式対応
      *  01.00.21    NACL-太田    03.06.24  診療所老人医療管理料（３０日
      *                                     以内再入院）の考慮を追加
      *  01.00.22    NACL-小豆沢  03.06.24  診療所老人医療管理料（３０日
      *                                     以内再入院）の考慮を追加
      *  01.00.23    NACL-小豆沢  03.06.24  特定集中治療室管理料の転室対応
      *  01.00.24    NACL-太田    03.07.01  出力先プリンタサブ追加
      *  01.00.25    NACL-小豆沢  03.07.08  特定集中治療室管理料の退院取消対応
      *  01.00.26    NACL-小豆沢  03.07.09  特定集中治療室管理料の異動取消対応
      *  01.00.27    NACL-小豆沢  03.07.10  退院取消時に診療録管理体制加算を
      *                                     算定しない
      *  01.00.28    NACL-小豆沢  03.07.10  退院取消時の通算日数算定を修正
      *  01.00.29    NACL-太田    03.07.10  排他制御処理修正
      *  01.00.30    NACL-小豆沢  03.07.10  異動取消時に診療会計を削除しないよう
      *                                     変更
      *  01.00.31    NACL-小豆沢  03.07.11  診療録管理体制加算の特別チェック削除
      *  01.00.32    NACL-小豆沢  03.09.24  特別入院基本料算定時の乳幼児加算対応
      *  01.00.33    NACL-太田    03.09.24  小児入院医療管理料での入院時に年齢
      *                                     をチェックし１５才以上であれば特定入
      *                                     院料コンボに表示をしない
      *  01.00.34    NACL-小豆沢  03.09.24  小児入院医療管理料３の算定対応追加
      *  01.00.35    NACL-多々納  03.09.30  業務権限判定追加
      *  01.00.36    NACL-小豆沢  03.10.03  一類感染症患者入院医療管理の算定対応
      *                                     追加
      *  01.00.37    NACL-小豆沢  03.10.03  老人性痴呆疾患治療病棟入院料の算定
      *                                     対応追加
      *  01.00.38    NACL-太田    03.10.09  入院日変更時に室料差額を初期化しない
      *  01.00.39    NACL-小豆沢  03.10.24  労災（自賠）入院時に老人点数無効
      *  01.00.40    NACL-小豆沢  03.10.27  労災（自賠）入院時に選定入院なし
      *  01.00.41    NACL-太田    03.11.14  更新日付編集対応
      *  01.00.42    NACL-小豆沢  03.11.26  老人患者の判定条件追加
      *  01.00.43    NACL-小豆沢  03.12.01  広範囲熱傷特定集中治療室管理料の
      *                                     算定対応追加（算定上限日数経過後の
      *                                     一般入院料算定はコメントとする）
      *  01.00.44    NACL-太田    04.01.28  病室変更時に室料差額表示が
      *                                     変わらない不具合を修正
      *  01.00.45    NACL-小豆沢  04.03.10  老人性痴呆疾患治療病棟入院料２の
      *                                     算定対応追加
      *  01.00.46    NACL-小豆沢  04.03.11  ハイケアユニット入院医療管理料の
      *                                     算定対応追加
      *  01.00.47    NACL-太田    04.03.11  転科・転棟・転室時の項目変更
      *                                     チェックを削除
      *  01.00.48    NACL-太田    04.03.12  算定入院料の表示を追加
      *  01.00.49    NACL-小豆沢  04.03.15  広範囲熱傷特定集中治療室管理料の
      *                                     算定対応再追加
      *  01.00.50    NACL-小豆沢  04.03.15  亜急性期入院医療管理料の算定対応追加
      *  01.00.51    NACL-小豆沢  04.03.16  １５歳未満の患者は選定入院対象外
      *  01.00.52    NACL-太田    04.08.02  定期請求中は退院登録は不可とする
      *  01.00.53    NACL-太田    04.10.14  退院事由追加
      *  01.00.54    NACL-小豆沢  05.07.19  自費１０項目対応
      *  01.00.55    NACL-小豆沢  06.01.13  退院日以降の診療会計を残す処理
      *                                     を可能とする区分を追加
      *  02.07.00    NACL-太田    06.02.15  入院オーダー確認画面遷移制御追加
      *平成１８年４月改定対応
      *  02.08.01    NACL-小豆沢  06/03/16  入院料老人コード置き換えは療養病棟
      *                                     または療養病床のみを対象とする
      *              NACL-小豆沢  06/03/17  特定入院料対応
      *                                     脳卒中ケアユニット入院医療管理料
      *                                     精神科救急入院料
      *                                     精神科急性期治療病棟入院料１
      *                                     精神科急性期治療病棟入院料２
      *                                     看護補助加算の変更
      *                                     異動処理時の食事退避処理変更
      *  02.09.01    NACL-小豆沢  06/05/16  老人特定入院基本料を特定入院料
      *                                     より外す
      *平成１８年７月改定対応
      *  03.00.01    NACL-小豆沢  06/06/19  療養病棟、療養病床改定対応
      *  03.04.01    NACL-小豆沢  06/12/15  異動処理時の外泊退避処理追加
      *  03.05.00    NACL-太田    07/05/10  グループ診療対応
      *  03.05.00    NACL-小豆沢  07/10/19  異動取消での食事退避対応
      *  04.01.00    NACL-太田    07/11/20  公害保険対応
      *  04.01.00    NACL-太田    07/11/20  室料差額６桁対応
      *  04.01.00    NACL-小豆沢  07/11/27  脳卒中ケアユニット入院医療管理料
      *                                     通算日数判定を変更
      *  04.01.00    NACL-太田    07/12/05  食事時間区分追加
      *  04.01.00    NACL-小豆沢  07/12/10  食事時間区分追加
      *  04.01.00    NACL-小豆沢  08/01/17  介護施設からの急性増悪による
      *                                     転入区分追加
      *  04.01.00    NACL-小豆沢  08/01/31  精神科急性期治療病棟入院料１、２
      *                                     の異動処理変更
      *平成２０年４月改定対応
      *  04.02.00    NACL-小豆沢  08/03/05  救命救急入院料の変更（入院登録のみ）
      *                                     亜急性期入院医療管理料２（全般）
      *                                     精神科救急入院料２
      *                                     小児入院医療管理料１
      *                                     回復期リハビリテーション入院料１
      *                                     回復期リハビリテーション入院料２
      *                                     精神科救急・合併症入院料
      *                                     短期滞在手術基本料３
      *  04.02.00    NACL-太田    08/03/06  後期高齢者判定追加
      *  04.02.00    NACL-小豆沢  08/03/07  シス管病棟設定の加算判定変更
      *  04.02.00    NACL-小豆沢  08/06/26  他院（特別な関係の医療機関）通算日数
      *                                     カウント変更
      *  04.03.00    NACL-小豆沢  08/07/08  褥瘡患者管理加算対応
      *  04.03.00    NACL-小豆沢  08/08/11  特定入院料コード取得をサブプロに変更
      *  04.07.00    NACL-小豆沢  11.07.25  救急・在宅等支援療養病床初期
      *                                     加算のシステム管理設定追加
      *  04.07.00    NACL-小豆沢  11/08/01  褥瘡患者管理加算の異動処理時
      *                                     コンボ対応
      *平成２６年４月改定対応
      *  00.00.00    NACL-小豆沢  14/02/27  有床診療所入院基本料１
      *                                     有床診療所入院基本料２
      *                                     有床診療所入院基本料３追加対応
      *平成３０年４月改正対応
      *  05.00.00    NACL-小豆沢  18/03/05  療養病棟の読み替えコード追加
      *  05.00.00    NACL-小豆沢  18/03/07  短期滞在手術等基本料の判定変更
      *  05.00.00    NACL-小豆沢  18/03/20  在宅患者支援療養病床初期加算（療養病棟入院基本料）
      *                                     在宅患者支援病床初期加算（地域包括ケア病棟入院料）
      *  05.00.00    NACL-小豆沢  18/04/18  食事個別指示対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    入退院登録共通領域
           COPY    "I0COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "I0SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-ENTRYCHK                PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-HKNCOMBICHK             PIC 9(01).
           03  FLG-SYORIPTN                PIC 9(01).
           03  FLG-TAISYO                  PIC 9(01).
           03  FLG-UKETUKEGAITO            PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-ORCSDAY                 PIC 9(01).
           03  FLG-JYUHKNCOMBISET          PIC 9(01).
           03  FLG-NYUKSN                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-TENKA-SRYKA             PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-ZI-SAISIN               PIC 9(01).
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-SYUMEI                  PIC 9(01).
           03  FLG-PTTEIKIRRK              PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-NYUKSNCHK               PIC 9(01).
           03  FLG-SYUARI                  PIC 9(01).
           03  FLG-RJN                     PIC 9(01).
           03  FLG-LIFE-RJN                PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-NYUINACCTARI            PIC 9(01).
           03  FLG-WKPTNYUINRRK            PIC 9(01).
           03  FLG-HKNCOMBIGET             PIC 9(01).
           03  FLG-RRKCHK                  PIC 9(01).
           03  FLG-CMB-EDIT                PIC 9(01).
           03  FLG-NYUREKILST-ROW          PIC 9(01).
           03  FLG-SHONUM                  PIC 9(01).
           03  FLG-TAIINRRK                PIC 9(01).
           03  FLG-TEIKI-CHK               PIC 9(01).
           03  FLG-TAINDOUJITU             PIC 9(01).
           03  FLG-TAINTUSANMINUS          PIC 9(01).
           03  FLG-TARGET                  PIC 9(01).
           03  FLG-ROUJIN                  PIC 9(01).
           03  FLG-SYS5000                 PIC 9(01).
           03  FLG-SYS5001                 PIC 9(01).
           03  FLG-LASTSKJ                 PIC 9(01).
           03  FLG-HIT                     PIC 9(01).
           03  FLG-PASS                    PIC 9(01).
           03  FLG-BUNBENCHK               PIC 9(01).
           03  FLG-BUNBEN                  PIC 9(01).
           03  FLG-RYOTARGET               PIC 9(01).
           03  FLG-IPPAN                   PIC 9(01).
           03  FLG-IPPAN2                  PIC 9(01).
           03  FLG-NSRYSRH                 PIC 9(01).
           03  FLG-NRRKSRH                 PIC 9(01).
           03  FLG-SST                     PIC 9(01).
           03  FLG-RYOYOKHN                PIC 9(01).
           03  FLG-EDIT                    PIC 9(01).
           03  FLG-PTCONF                  PIC 9(01).
           03  FLG-IDOKAIKEI-CANCEL        PIC 9(01).
           03  FLG-SHOKUDOU                PIC 9(01).
           03  FLG-ASA                     PIC 9(01).
           03  FLG-HIRU                    PIC 9(01).
           03  FLG-YU                      PIC 9(01).
           03  FLG-ALT-OK                  PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(05).
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX6                        PIC 9(05).
           03  IDX7                        PIC 9(05).
           03  IDX8                        PIC 9(05).
           03  IDX9                        PIC 9(05).
           03  IDXA                        PIC 9(05).
           03  IDXL                        PIC 9(05).
           03  IDXM                        PIC 9(05).
           03  IDX10                       PIC 9(05).
           03  IDX11                       PIC 9(05).
           03  IDX12                       PIC 9(05).
           03  IDX13                       PIC 9(05).
           03  IDX14                       PIC 9(05).
           03  IDX15                       PIC 9(05).
           03  IDX16                       PIC 9(05).
           03  IDX17                       PIC 9(05).
           03  IDXK                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
           03  IDX-DAY                     PIC 9(02).
           03  IDX-SKJ-TAIHI               PIC 9(02).
           03  IDX-RRKBYO                  PIC 9(05).
           03  IDX-BTUNUM                  PIC 9(05).
           03  IDX-SYU                     PIC 9(05).
           03  IDX-MATSUBI                 PIC 9(02).
           03  IDX-LIFE                    PIC 9(05).
           03  IDX-KSN                     PIC 9(05).
           03  IDX-KSN2                    PIC 9(05).
           03  IDX-SST                     PIC 9(05).
           03  IDX-NSRY                    PIC 9(05).
           03  IDX-JOKU                    PIC 9(02).
           03  IDX-SHOKUDOU                PIC 9(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-TUKISU          PIC 9(05).
           03  CNT-KSN             PIC 9(05).
           03  CNT-SST             PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD     PIC X(08).
           03  SYS-TIME    PIC X(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDGET-STATE            PIC S9(9) BINARY.
           03  WRK-I0IDCD                  PIC X(04).
           03  WRK-I0ID-FLG                PIC X(02).
           03  WRK-I0IDMSG                 PIC X(80). 
           03  WRK-MOTOPG                  PIC X(08).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-KANACHK-MAE-INPUT       PIC X(400).
           03  WRK-KANACHK-OUT-INPUT       PIC X(400).
           03  WRK-DOUJITSU-FLG            PIC X(01).
           03  WRK-DOUJITSU-MOTOYMD        PIC X(08).
           03  WRK-CALC-YMD1.
               05  WRK-CALC-YY1            PIC 9(04).
               05  WRK-CALC-MM1            PIC 9(02).
               05  WRK-CALC-DD1            PIC 9(02).
      *
           03  WRK-CALC-YMD2.
               05  WRK-CALC-YY2            PIC 9(04).
               05  WRK-CALC-MM2            PIC 9(02).
               05  WRK-CALC-DD2            PIC 9(02).
           03  WRK-AGE-YY                  PIC S9(04).
           03  WRK-AGE-MM                  PIC S9(02).
           03  WRK-AGE-G.
               05  WRK-AGE-Z               PIC ---9.
               05  WRK-AGE-MEI             PIC X(04).
           03  WRK-BIRTHDAY                PIC X(08).
           03  WRK-CMB-CD                  PIC X(05).
           03  WRK-CMB-G.
               05  WRK-CMB-IDX             PIC 9(03).
               05  WRK-CMB-ITM             PIC X(100).
               05  WRK-CMB-ITM2            PIC X(100).
               05  WRK-CMB-ITM3            PIC X(100).
               05  WRK-CMB-ITM4            PIC X(100).
               05  WRK-CMB-ITM5            PIC X(100).
           03  WRK-NUM-EDIT.
               05  WRK-99                  PIC 9(02).
               05  WRK-99999               PIC 9(05).
               05  WRK-999999              PIC 9(06).
               05  WRK-ZZ9                 PIC ZZ9.
               05  WRK-ZZZ9                PIC ZZZ9.
               05  WRK-ZZZZ9               PIC ZZZZ9.
               05  WRK-ZZZZZ9              PIC ZZZZZ9.
           03  WRK-KBN1-G.
               05  WRK-KBN1                PIC 9(01).
           03  WRK-KBN2-G.
               05  WRK-KBN2                PIC 9(02).
           03  WRK-SAGAKU.
               05  WRK-SAGAKU-NUM          PIC ZZZZZ9.
               05  WRK-SAGAKU-EN           PIC X(02).
           03  WRK-STR1                    PIC X(20).
           03  WRK-STR2                    PIC X(20).
           03  WRK-IDX1                    PIC 9(05).
           03  WRK-IDX2                    PIC 9(05).
           03  WRK-BTUJYOHO-TBL.
               05  WRK-BTUJYOHO-CNT        PIC 9(05).
               05  WRK-BTUJYOHO-OCC        OCCURS  50.
                   07  WRK-BTUJY-BTUNUM    PIC X(02).
                   07  WRK-BTUJY-BRM-SAGAKU
                                           PIC X(02).
                   07  WRK-BTUJY-BRM-KA    PIC X(02).
           03  WRK-BRM-SAGAKU-CD           PIC X(02).
           03  WRK-BTU-KA-CD               PIC X(02).
           03  WRK-BRM-KA-CD               PIC X(02).
           03  WRK-HKNCOMBI                PIC X(100).
           03  WRK-FTNRATE                 PIC X(05).
           03  WRK-HKNKBN-MEI              PIC X(02).
           03  WRK-RRKNUM                  PIC 9(03).
           03  WRK-RRKEDANUM               PIC 9(03).
           03  WRK-KJNYMD                  PIC X(08).
           03  WRK-SRYYMD                  PIC X(08).
           03  WRK-KIHON-KJNYMD            PIC X(08).
           03  WRK-UPDKBN                  PIC X(3).
           03  WRK-BRMNUM                  PIC X(06).
           03  WRK-BRMNUM-IN               PIC X(06).
           03  WRK-BRMNUM-EDT              PIC X(06).
           03  WRK-BRMNUM-OT               PIC X(06).
           03  WRK-NYUINKA-KEY             PIC X(02).
           03  WRK-BTUNUM-KEY              PIC X(02).
           03  WRK-BRMNUM-KEY              PIC X(08).
           03  WRK-HKNCOMBI-KEY            PIC X(04).
           03  WRK-TOKUTEI-KEY.
               05  WRK-TOKUTEI-KBN-KEY     PIC X(01).
               05  WRK-TOKUTEI-NYUIN-KEY   PIC X(02).
      *
           03  WRK-REKI-BYOMEI-EDIT-AREA.
               05  WRK-REKI-BYOMEI-EDIT-OCC
                                           OCCURS 100.
                   07  WRK-REKI-BYO-ZITA-FLG   PIC 9(01).
                   07  WRK-REKI-BYO-NYUINKA    PIC X(02).
                   07  WRK-REKI-BYO-NYUINYMD   PIC X(08).
                   07  WRK-REKI-BYO-TAIINYMD   PIC X(08).
                   07  WRK-REKI-BYO-RENNUM     PIC 9(02).
                   07  WRK-REKI-BYO-SRYYMD     PIC X(08).
                   07  WRK-REKI-BYO-BYOMEI     PIC X(40).
                   07  WRK-REKI-BYO-TENKIKBN   PIC X(01).
           03  WRK-STYMD                       PIC X(08).
           03  WRK-EDYMD                       PIC X(08).
           03  WRK-NISSU                       PIC 9(05).
           03  WRK-NISSU1                      PIC 9(05).
           03  WRK-NISSU2                      PIC 9(05).
           03  WRK-TSUSAN                      PIC 9(05).
           03  WRK-NYUINNISSU                  PIC 9(05).
           03  WRK-STUSAN-NISSU03              PIC 9(05).
           03  WRK-SANTEISTYMD                 PIC X(08).
           03  WRK-SENTEI-STYMD                PIC X(08).
           03  WRK-BIRTHDAY-15                 PIC X(08).
           03  WRK-BIRTHDAY-15R   REDEFINES    WRK-BIRTHDAY-15.
               05  WRK-BIRTHDAY-15Y            PIC 9(04).
               05  WRK-BIRTHDAY-15MD           PIC 9(04).
           03  WRK-REKI-ZIIN-TBL.
               05  WRK-REKI-ZIIN-CNT           PIC 9(05).
               05  WRK-REKI-ZIIN-OCC           OCCURS  100.
                   07  WRK-REKI-ZIIN-NYUINYMD  PIC X(08).
                   07  WRK-REKI-ZIIN-TAIINYMD  PIC X(08).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-HKNCOMBINUM             PIC X(04).
           03  WRK-HKNCOMBINUM9   REDEFINES  WRK-HKNCOMBINUM
                                           PIC 9(04).
           03  WRK-SHONUM                  PIC 9(03).
           03  WRK-SRYKAMEI                PIC X(30).
           03  WRK-TENKA-SRYKA-TBL.
               05  WRK-TENKA-SRYKA-CNT     PIC 9(05).
               05  WRK-TENKA-SRYKA         PIC X(02)
                                           OCCURS 50.
           03  WRK-SAGAKU-KBN              PIC X(02).
           03  WRK-SAGAKU-KBN9  REDEFINES  WRK-SAGAKU-KBN
                                           PIC 9(02).
           03  WRK-KANRICD                 PIC X(04).
           03  WRK-KBNCD                   PIC X(08).
           03  WRK-YUKYMD                  PIC X(08).
           03  WRK-TOKNYUIN-NM             PIC X(60).
           03  WRK-TOKNYUIN.
               05  WRK-TOKNYUIN-KBN        PIC X(01).
               05  WRK-TOKNYUIN-CD         PIC X(02).
           03  WRK-CHKTOKNYUIN.
               05  WRK-CHKTOKNYUIN-KBN     PIC X(01).
               05  WRK-CHKTOKNYUIN-CD      PIC X(02).
           03  WRK-TOKNYUIN-SRYCD-X.
               05  WRK-TOKNYUIN-SRYCD      PIC 9(09).
           03  WRK-TOKNYUIN-SRYCDTTL       PIC 9(14).
           03  WRK-TOKNYUIN-NYUINRYOKBN    PIC X(02).
           03  WRK-TAIHI-SRYKA             PIC X(02).
           03  WRK-NOW-SKYSTYMD            PIC X(08).
           03  WRK-SKYSTYM                 PIC X(06).
           03  WRK-SYU-SUM-IDX             PIC 9(05).
           03  WRK-TAIINUPDCNT             PIC 9(05).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-IDOYMD                  PIC X(08).
           03  WRK-NYUINYMD                PIC X(08).
           03  WRK-TENNYUYMD               PIC X(08).
           03  WRK-TENNYUYMD9   REDEFINES  WRK-TENNYUYMD.
               05  WRK-TENNYUYMD-YY9       PIC 9(04).
               05  WRK-TENNYUYMD-MM9       PIC 9(02).
               05  WRK-TENNYUYMD-DD9       PIC 9(02).
           03  WRK-MAE-SKYEDYMD            PIC X(08).
           03  WRK-SKYSTYMD                PIC X(08).
           03  WRK-SKYEDYMD                PIC X(08).
      *    医療機関設定の入院料加算チェック用
           03  WRK-NYUKHNKSN-SRYCD         PIC X(09)  OCCURS 20.
           03  WRK-KHN-NYUKSNKBN           PIC X(03).
           03  WRK-KSN-NYUKSNKBN           PIC X(03).
           03  WRK-NYUKSNKBN               PIC X(03).
           03  WRK-STYUKYMD                PIC X(08).
           03  WRK-NAI-TAIINYMD            PIC X(08).
           03  WRK-TAIINYMD.
               05  WRK-TAIINYM             PIC X(06).
               05  WRK-TAIINDD             PIC 9(02).
           03  WRK-MCP-PUTTYPE             PIC X(08).
           03  WRK-ORCHCN01-SYORIKBN       PIC X(01).
           03  WRK-ORCHCN01-PRTKBN         PIC X(01).
           03  WRK-AGECHK-SRYYMD           PIC X(08).
           03  WRK-NACCT-SRYYM             PIC X(08).
           03  WRK-PRINT-PG                PIC X(20).
           03  WRK-ORCSHKN-HKNCOMBINUM     PIC X(04).
           03  WRK-TOKUBETSU-NYUIN         PIC X(01).
      *    会計作成用退避領域
           03  WRK-TAIHI-HKN               PIC 9(03).
           03  WRK-TAIHI-SHOKUJI           PIC 9(03).
           03  IDX-SHOKUJIX                PIC X(02).
           03  IDX-SHOKUJI9   REDEFINES    IDX-SHOKUJIX
                                           PIC 9(02).
           03  IDX-SKJ                     PIC 9(02).
           03  WRK-NYUINCHU-TAIINYMD       PIC X(08).
           03  WRK-ORCHCN01-HKNCOMBINUM    PIC 9(04).
           03  WRK-STIDX                   PIC 9(02).
           03  WRK-EDIDX                   PIC 9(02).
           03  WRK-MIN-YMD.
               05  WRK-MIN-YM              PIC X(06).
               05  WRK-MIN-DD              PIC 9(02).
      *    特定入院料の会計作成後に一般の入院料で会計を作成するための
      *    領域
           03  IPN-NYUKAIKEI-SAKUSEI-FLG   PIC X(01).
           03  IPN-SANTEISTYMD             PIC X(08).
           03  WRK-IPN-HANTEI-YMD1.
               05  WRK-IPN-HANTEI-YM1.
                   07  WRK-IPN-HANTEI-Y1   PIC 9(04).
                   07  WRK-IPN-HANTEI-M1   PIC 9(02).
               05  WRK-IPN-HANTEI-D1       PIC 9(02).
           03  WRK-IPN-HANTEI-YMD2         PIC X(08).
      *    特定入院料の会計作成用に診療コードを退避する領域
           03  WRK-TOKUTEI-SRYCD1          PIC X(09).
           03  WRK-TOKUTEI-SRYCD2          PIC X(09).
           03  WRK-TOKUTEI-SRYCD3          PIC X(09).
           03  WRK-TOKUTEI-SRYCD4          PIC X(09).
           03  WRK-SRYCD                   PIC X(09).
           03  WRK-SHOKAI-NYUINYMD-W       PIC X(10).
           03  WRK-CHK-SHONUM              PIC 9(03).
           03  WRK-I01-I04-PROCKBN         PIC X(01).
           03  WRK-FTN-NYUINYMD            PIC X(08).
           03  WRK-FTN-TAIINYMD            PIC X(08).
           03  WRK-FTN-DJKBN               PIC X(01).
           03  WRK-FTN-PROCKBN             PIC X(01).
           03  WRK-FTN-RRKNUM              PIC 9(03).
           03  WRK-NYUREKILST-SEL          PIC 9(05).
      *    転科／転棟／転室時に食事会計を退避する項目（最大２カ月分）
      *    外泊も追加(VER3.4)
           03  WRK-TENROOM-SKJ-SRYYMX.
               05  WRK-TENROOM-SKJ-SRYYY9  PIC 9(04).
               05  WRK-TENROOM-SKJ-SRYMM9  PIC 9(02).
           03  WRK-TENROOM-SKJ-TABLE.
               05  WRK-TENROOM-SKJ-TBL     OCCURS  2.
                   07  WRK-TENROOM-SKJ-SRYYM
                                           PIC X(06).
                   07  WRK-TENROOM-SKJ-DD  PIC 9(02).
                   07  WRK-TENROOM-SKJ     PIC 9(03)  OCCURS  31.
                   07  WRK-TENROOM-SKJ18   PIC 9(03)  OCCURS  31.
                   07  WRK-TENROOM-SKJ19   PIC 9(03)  OCCURS  31.
                   07  WRK-TENROOM-SKJ20   PIC 9(03)  OCCURS  31.
                   07  WRK-TENROOM-GAIHAKU PIC 9(03)  OCCURS  31.
           03  WRK-TENROOM-MAE-SKJ1        PIC 9(03).
           03  WRK-TENROOM-MAE-SKJ2        PIC 9(03).
           03  WRK-TENROOM-MAE-SKJ1-ASA    PIC 9(03).
           03  WRK-TENROOM-MAE-SKJ1-HIRU   PIC 9(03).
           03  WRK-TENROOM-MAE-SKJ1-YORU   PIC 9(03).
           03  WRK-TENROOM-MAE-SKJ2-ASA    PIC 9(03).
           03  WRK-TENROOM-MAE-SKJ2-HIRU   PIC 9(03).
           03  WRK-TENROOM-MAE-SKJ2-YORU   PIC 9(03).
      *    転科／転棟／転室時に食堂加算を退避する項目
           03  WRK-SHOKUDOU-AREA.
               05  WRK-SHOKUDOU-SRYYM.
                   07  WRK-SHOKUDOU-SRYYY  PIC 9(04).
                   07  WRK-SHOKUDOU-SRYMM  PIC 9(02).
               05  WRK-SHOKUDOU-SKJTBL.
                   07  WRK-SHOKUDOU-SKJ-ARI PIC X(01)  OCCURS  31.
      *
           03  WRK-NAI-NYUINYMD.
               05  WRK-NAI-NYUINYM         PIC X(06).
               05  WRK-NAI-NYUINDD         PIC 9(02).
           03  WRK-CHK-YMD                 PIC X(08).
           03  WRK-CMB-TOKNYUIN-AREA           PIC X(3000).
           03  WRK-ERRMSG                  PIC X(10).
           03  WRK-SKYMONEY                PIC S9(08).
           03  WRK-RJNYMD                  PIC X(08).
           03  WRK-LASTSRYYM               PIC X(06).
           03  WRK-KSNNAME                 PIC X(100).
           03  WRK-KSN-G.
               05  WRK-KSN-OCC             OCCURS  3.
                   07  WRK-KSN-SRYCD       PIC X(09).
                   07  WRK-KSN-CD          PIC X(01).
           03  WRK-CNT                     PIC 9(05).
           03  WRK-TOKNYUINCD              PIC X(03).
           03  WRK-TOKHIGAITO              PIC X(01).
           03  WRK-BUNBENRRKNUM            PIC 9(03).
      *
      *    転科／転棟／転室時に異動日の食事会計を退避する項目
      *    （入院日食事時間開始区分の判定による更新の為）
           03  WRK-IDOBI-SKJ-SRYYMD.
               05  WRK-IDOBI-SKJ-SRYYY9    PIC 9(04).
               05  WRK-IDOBI-SKJ-SRYMM9    PIC 9(02).
               05  WRK-IDOBI-SKJ-SRYDD9    PIC 9(02).
           03  WRK-IDOBI-SKJ18             PIC 9(03).
           03  WRK-IDOBI-SKJ19             PIC 9(03).
           03  WRK-IDOBI-SKJ20             PIC 9(03).
      *
      *    食堂加算判定領域
           03  WRK-SHOKUJI-ASA             PIC 9(01).
           03  WRK-SHOKUJI-HIRU            PIC 9(01).
           03  WRK-SHOKUJI-YORU            PIC 9(01).
      *
      *    褥瘡患者管理加算判定領域
           03  WRK-JOKUSOU-IDXX            PIC X(02).
           03  WRK-JOKUSOU-IDX9   REDEFINES   WRK-JOKUSOU-IDXX
                                           PIC 9(02).
           03  WRK-JOKUSOU-IDXX2           PIC X(02).
           03  WRK-JOKUSOU-IDX92   REDEFINES   WRK-JOKUSOU-IDXX2
                                           PIC 9(02).
      *
      *    前月末日算定用
           03  WRK-CHK-SRYYM               PIC X(06).
           03  WRK-CHK-SRYYMR      REDEFINES  WRK-CHK-SRYYM.
               05  WRK-CHK-SRYYR           PIC 9(04).
               05  WRK-CHK-SRYMR           PIC 9(02).
      *
      *    入院会計カレンダーチェック用
           03  WRK-CHK2-SRYYM              PIC X(06).
           03  WRK-CHK2-SRYYMR     REDEFINES  WRK-CHK2-SRYYM.
               05  WRK-CHK2-SRYYR          PIC 9(04).
               05  WRK-CHK2-SRYMR          PIC 9(02).
           03  WRK-CHK2-FLG                PIC 9(01).
      *
      *    翌月入院会計作成用
           03  WRK-SAI-SRYYM               PIC X(06).
           03  WRK-SAI-SRYYMR      REDEFINES  WRK-SAI-SRYYM.
               05  WRK-SAI-SRYYR           PIC 9(04).
               05  WRK-SAI-SRYMR           PIC 9(02).
           03  FLG-SAI-SAKUSEI             PIC 9(01).
           03  FLG-SAI-SAKUSEI2            PIC 9(01).
           03  WRK-SAI-I01-TOKNYUIN-CD     PIC X(02).
      *
      *    出生児体重
           03  WRK-WEIGHT                  PIC 9(04).
           03  WRK-ERRCD.
               05  WRK-ERRCD-NUM           PIC 9(04).
           03  WRK-SKJKBN-MAE              PIC X(01).
           03  WRK-TIMEKBN-PATTERN         PIC X(01).
      *
      *    患者年齢算出用エリア
       01  WRK-NENREI-AREA.
           03  WRK-NENREI                  PIC 9(03).
           03  WRK-YMDS1.
               05  WRK-YYS1                PIC 9(04).
               05  WRK-MMS1                PIC 9(02).
               05  WRK-DDS1                PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2                PIC 9(04).
               05  WRK-MMS2                PIC 9(02).
               05  WRK-DDS2                PIC 9(02).
           03  WRK-DOUJITUFLG-ST           PIC X(01).
           03  WRK-DOUJITUFLG-ED           PIC X(01).
           03  WRK-ORG-PRINT-PG            PIC X(20).
           03  WRK-TOKHIGAITO-G.
               05  WRK-TOKHIGAITO-CD       PIC X(09).
               05  WRK-TOKHIGAITO-NAME     PIC X(60).
           03  WRK-HOSPSBT                 PIC 9(02).
           03  WRK-I01-SYORIKBN-CD         PIC X(02).
      *
      *    診療所老人医療管理料（転科、転棟）計算エリア
       01  TN-IDX                          PIC 9(02).
       01  TN-IDX2                         PIC 9(02).
       01  WK-TOKUTEI-DAY                  PIC 9(04).
       01  WK-TOKUTEI-DAY2                 PIC 9(04).
      *
       01  WRK-ROW.
           03  WRK-NYUREKILST-ROW          PIC S9(9)   BINARY.
      *
       01  CONST-AREA.
           03  CONST-H280401               PIC X(08)   VALUE "20160401".
           03  CONST-H261001               PIC X(08)   VALUE "20141001".
           03  CONST-H260401               PIC X(08)   VALUE "20140401".
           03  CONST-H241001               PIC X(08)   VALUE "20121001".
           03  CONST-H240401               PIC X(08)   VALUE "20120401".
           03  CONST-H220401               PIC X(08)   VALUE "20100401".
           03  CONST-H211001               PIC X(08)   VALUE "20091001".
           03  CONST-H181001               PIC X(08)   VALUE "20061001".
           03  CONST-H180401               PIC X(08)   VALUE "20060401".
           03  CONST-H180701               PIC X(08)   VALUE "20060701".
           03  CONST-H200401               PIC X(08)   VALUE "20080401".
           03  CONST-SYORIKBN-G.
               05  CONST-SYORIKBN-VAL.
      *
                   07  CONST-NYUIN-G.
                       09  CONST-NYUIN
                               PIC X(02)   VALUE   "01".
                       09  CONST-NYUINFL
                               PIC X(01)   VALUE   SPACE.
                       09  CONST-NYUINMEI
                               PIC X(30)   VALUE   "入院登録".
      *
                   07  CONST-NYUINALT-G.
                       09  CONST-NYUINALT
                               PIC X(02)   VALUE   "01".
                       09  CONST-NYUINALTFL
                               PIC X(01)   VALUE   SPACE.
                       09  CONST-NYUINALTMEI
                               PIC X(30)   VALUE   "入院登録（訂正）".
      *
                   07  CONST-TAIIN-G.
                       09  CONST-TAIIN
                               PIC X(02)   VALUE   "02".
                       09  CONST-TAIINFL
                               PIC X(01)   VALUE   SPACE.
                       09  CONST-TAIINMEI
                               PIC X(30)   VALUE   "退院登録".
      *
                   07  CONST-HENKOU-G.
                       09  CONST-HENKOU
                               PIC X(02)   VALUE   "03".
                       09  CONST-HENKOUFL
                               PIC X(01)   VALUE   SPACE.
                       09  CONST-HENKOUMEI
                               PIC X(30)   VALUE   "変更".
      *
                   07  CONST-SYOUKAI-G.
                       09  CONST-SYOUKAI
                               PIC X(02)   VALUE   "04".
                       09  CONST-SYOUKAIFL 
                               PIC X(01)   VALUE   SPACE.
                       09  CONST-SYOUKAIFLMEI 
                               PIC X(30)   VALUE   "照会".
      *
                   07  CONST-NYUINTORIKESHI-G.
                       09  CONST-NYUINTORIKESHI
                               PIC X(02)   VALUE   "05".
                       09  CONST-NYUINTORIKESHIFL
                               PIC X(01)   VALUE   SPACE.
                       09  CONST-NYUINTORIKESHIMEI
                               PIC X(30)   VALUE   "入院取消".
      *
                   07  CONST-NYUINTORIKESHIALL-G.
                       09  CONST-NYUINTORIKESHIALL
                               PIC X(02)   VALUE   "06".
                       09  CONST-NYUINTORIKESHIALLFL
                               PIC X(01)   VALUE   SPACE.
                       09  CONST-NYUINTORIKESHIALLMEI
                               PIC X(30)   VALUE "入院取消（会計含む）".
      *
                   07  CONST-TAIINTORIKESHI-G.
                       09  CONST-TAIINTORIKESHI
                               PIC X(02)   VALUE   "07".
                       09  CONST-TAIINTORIKESHIFL
                               PIC X(01)   VALUE   SPACE.
                       09  CONST-TAIINTORIKESHIMEI
                               PIC X(30)   VALUE   "退院取消".
      *
                   07  CONST-IDO-G.
                       09  CONST-IDO
                               PIC X(02)   VALUE   "08".
                       09  CONST-IDOFL
                               PIC X(01)   VALUE   SPACE.
                       09  CONST-IDOMEI
                               PIC X(30)   VALUE   "転科　転棟　転室".
      *
                   07  CONST-IDOTORIKESHI-G.
                       09  CONST-IDOTORIKESHI
                               PIC X(02)   VALUE   "09".
                       09  CONST-IDOTORIKESHIFL
                               PIC X(01)   VALUE   SPACE.
                       09  CONST-IDOTORIKESHIMEI
                               PIC X(30)   VALUE   "異動取消".
      *
                   07  CONST-SAIKEISAN-G.
                       09  CONST-SAIKEISAN
                               PIC X(02)   VALUE   "10".
                       09  CONST-SAIKEISANFL
                               PIC X(01)   VALUE   SPACE.
                       09  CONST-SAIKEISANMEI
                               PIC X(30)   VALUE   "退院再計算".
      *
                   07  CONST-TAIINSRYHZN-G.
                       09  CONST-TAIINSRYHZN
                               PIC X(02)   VALUE   "11".
                       09  CONST-TAIINSRYHZNFL
                               PIC X(01)   VALUE   SPACE.
                       09  CONST-TAIINSRYHZNMEI
                               PIC X(30)   VALUE "退院登録（診療保存）".
           03  CONST-SKJRYOYO2             PIC X(09)  VALUE "197000710".
           03  CONST-NYUREKILST-MAX        PIC 9(05)   VALUE   100.
           03  CONST-TOKNYUIN-MAX          PIC 9(05)   VALUE   100.
           03  CONST-JOB-SHELLID           PIC X(08)   VALUE   "TEIKI1".
           03  CONST-JOB-JOBID             PIC 9(07)   VALUE   1.
           03  CONST-GMN-KSN-MAX           PIC 9(05)   VALUE   3.
           03  CONST-KSNCD-G.
               05  CONST-KSNCD-MAX         PIC 9(05)   VALUE   8.
               05  CONST-KSNCD-VAL.
                   07  CONST-KSNCD-190116990
                                   PIC X(09)   VALUE   "190116990".
                   07  CONST-KSNCD-190135470
                                   PIC X(09)   VALUE   "190135470".
                   07  CONST-KSNCD-190134270
                                   PIC X(09)   VALUE   "190134270".
                   07  CONST-KSNCD-190135670
                                   PIC X(09)   VALUE   "190135670".
                   07  CONST-KSNCD-190142970
                                   PIC X(09)   VALUE   "190142970".
                   07  CONST-KSNCD-190143370
                                   PIC X(09)   VALUE   "190143370".
                   07  CONST-KSNCD-190152470
                                   PIC X(09)   VALUE   "190152470".
                   07  CONST-KSNCD-190177170
                                   PIC X(09)   VALUE   "190177170".
               05  CONST-KSNCD-R REDEFINES CONST-KSNCD-VAL.
                   07  CONST-KSNCD-OCC     OCCURS  8.
                       09  CONST-KSNCD     PIC X(09).
           03  CONST-KANSATU               PIC X(02)    VALUE "10".
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                               //SPA-// BY //WRK-SPA-//.
      *
      *    入院履歴ワーク
       01  WKPTNYUIN-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING
                               //PTNYUINRRK-// BY //WKPTNYUIN-//.
      *
      *    ユーザープログラム用入院履歴退避領域
       01  USERPGRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING
                               //PTNYUINRRK-// BY //USERPGRRK-//.
      *
      *    収納ワーク
       01  SUMSYUNOU-REC.
         02 SUMSYUNOU-CNT     PIC 9(05).
         02 SUMSYUNOU-OCC     OCCURS 13.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //SUMSYU-//.
         02 SUMSYUDAY-OCC     OCCURS 13.
           COPY  "CPSYUDAY.INC"    REPLACING  //SDAY-//
                                                       BY //SUMSDAY-//.
      *    収納合計ワーク
       01  TTLSYUNOU-REC.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //TTLSYU-//.
      *
      *    退院日の同日再入院用ワーク領域
       01  WRK-DOU-AREA.
           03  WRK-DOU-NAI-I01-NYUINYMD      PIC X(08).
           03  WRK-DOU-PTNYUINRRK-TENNYUYMD  PIC X(08).
           03  WRK-DOU-TENNYUYMD             PIC X(08).
           03  FLG-JIINRRK                   PIC 9(01).
       01  WRKRRK-REC.
           COPY  "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                            BY  //WRKRRK-//.
       01  LASTRRK-REC.
           COPY  "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                            BY  //LASTRRK-//.
       01  BAKRRK-REC.
           COPY  "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                            BY  //BAKRRK-//.
       01  NRRK-REC.
           COPY  "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                            BY  //NRRK-//.
      *
           COPY    "CPRYOYOCD.INC".
      *
           COPY    "CPLNKS08.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    （診療科入院科情報）
           COPY    "CPSK1005.INC".
      *    （職員情報）
           COPY    "CPSK1010.INC".
      *
      *    帳票プログラム名
           COPY    "CPSK1035.INC".
      *    （医療機関情報）
           COPY    "CPSK5000.INC".
      *    （病棟情報）
           COPY    "CPSK5001.INC".
      *    （病室情報）
           COPY    "CPSK5002.INC".
      *    （短期３）
           COPY    "CPSK5003.INC".
      *    （室料差額）
           COPY    "CPSK5113.INC".
      *    （特定入院料−病棟）
           COPY    "CPSK5110.INC".
      *    （特定入院料−病室）
           COPY    "CPSK5111.INC".
      *    医療観察入院医学管理料
           COPY    "CPSK5112.INC".
      *    
           COPY    "CPSK5013.INC".
      *    
           COPY    "CPSK9000.INC".
      *    
           COPY    "CPSK9700.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    患者他院歴付加情報
       01  PTTAINFUKA-REC.
           COPY    "CPPTTAINFUKA.INC".
      *
      *    受付マスタ
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスター
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
       01  WKJYURRK-REC.
           COPY    "CPJYURRK.INC"      REPLACING
                                       //JYURRK-// BY //WKJYURRK-//.
      *    老人一般コード変換
        01  SRYCDCHG-REC.
            COPY    "CPSRYCDCHG.INC".
      *
      *    患者定期請求履歴
        01 PTTEIKIRRK-REC.
           COPY     "CPPTTEIKIRRK.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入院料加算チェック
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    選定療養費
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    入院診療行為照会テーブル
       01  NSRYSRH-REC.
           COPY    "CPNSRYSRH.INC".
      *
      *    入院履歴照会テーブル
       01  NRRKSRH-REC.
           COPY    "CPNRRKSRH.INC".
      *
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *    患者個別設定
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    同日入院チェックサブ
           COPY    "CPORCSDNTCHK.INC".
      *
       01  LNKNRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-// 
                                       BY         //LNKNRRK-//.
      *
      *    入院登録サブ
           COPY    "CPI0SUB01.INC".
           COPY    "CPI0SUB02.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    数値号変換サブ
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    初期加算算定サブ
           COPY    "CPORCSSYOKIKSN.INC".
      *
      *    入院会計作成サブ
           COPY    "CPORCSNYUACCT.INC".
      *
      *    入院料加算算定可否チェックサブ
           COPY    "CPORCSI01.INC".
      *
      *    入退院登録PUSH通知
           COPY    "CPORCSI03.INC".
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
      *    負担金計算サブエラーメッセージ
           COPY    "CPORCS02MSG.INC".
      *
      *    通算日数取得サブ
           COPY "CPORCSTUSAN.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    入院カルテ
           COPY    "CPORCHCN01.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    保険組合せ編集サブ
           COPY    "CPORCSHKNSET.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日報キー取得サブ
           COPY "CPORCSDAILYKEY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    定期請求ジョブチェック
           COPY    "CPORCSTEIKIJOB.INC".
      *
      *    主科情報アクセス用パラメタ
       01  ORCSSYUACCAREA.
           COPY    "CPORCSSYUACC.INC".
      *
      *    病棟番号編集処理
           COPY    "CPORCSBRMNUM.INC".
      *
      *    収納更新差額取得
           COPY  "CPORCSS002.INC".
      *
           COPY  "CPORCSS010.INC".
      *
      *    入院料点数チェックサブ
           COPY  "CPORCSS007.INC".
      *
      *    ユーザプログラム起動サブパラメタ
           COPY    "CPORCSUSERCHK.INC".
      *
      *    特定入院料コード取得サブ
           COPY  "CPORCSTOKNYUIN.INC".
      *
      *    医療観察入院料コード取得パラメタ
           COPY  "CPORCSIRYOKNS.INC".
      *
      *    病名編集
           COPY  "CPORCSMKBYOMEI.INC".
      *
      *    亜急性期入院医療管理料判定パラメタ
           COPY  "CPORCSAKYUSEI.INC".
      *
      *    特定入院料算定期間取得パラメタ
           COPY  "CPORCSTOKNYUIN2.INC".
      *
      *    入院料加算取得パラメタ
           COPY  "CPORCSNYUKSNGET.INC".
      *
      *    夜勤時間超過減算入院料コード取得サブ
           COPY "CPORCSYAKINGEN.INC".
      *
      *    夜勤時間特別入院基本料コード取得サブ
           COPY "CPORCSYAKINTOKU.INC".
      *
      *    看護職員数等入院基本料コード取得サブ
           COPY "CPORCSKANGOCHG.INC".
      *
      *    施設基準取得
           COPY "CPORCSS014.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *     テーブルアクセス
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA31SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-I0FREE.
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD.
           MOVE    ZERO            TO  FLG-END.
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF  FLG-END   =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON.
           MOVE    SPA-I0FREE      TO  SPA-FREE
           .
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "I0ERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
      *
               IF    ( SPA-I0IDCD  NOT =   SPACE )
                   PERFORM 520-I0IDSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
      *
               IF    ( SPA-I0IDCD2 NOT =   SPACE )
                   PERFORM 520-I0IDSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
      *
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "I01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
               IF      WRK-MOTOPG          =   "P97" OR "XD01"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-I01
                   MOVE    SPA-WORK        TO  SPA-I0KYOUTU
                   MOVE    1               TO  SPA-GMN-I01-CUR
               END-IF
      *
               EVALUATE    WRK-MOTOPG
               WHEN    "XD01"
                   PERFORM 310-SPASET-SEC
                   PERFORM 420-MAEPTINF-SET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "P97"
                   PERFORM 330-P97-SET-SEC
                   GO  TO  300-SCREEN-EXT
               END-EVALUATE
      *
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *
      *    確認画面
           WHEN    "I0ID1"
               PERFORM 330-I0ID1-SET-SEC
      *
               GO  TO      300-SCREEN-EXT
      *
      *
      *    確認画面２
           WHEN    "I0ID2"
               PERFORM 330-I0ID2-SET-SEC
      *
               GO  TO      300-SCREEN-EXT
      *
      *    入院歴表示画面
           WHEN    "I02"
               PERFORM 330-I02-SET-SEC
               GO  TO      300-SCREEN-EXT
      *
      *    入院歴作成画面
           WHEN    "I03"
               PERFORM 330-I03-SET-SEC
               GO  TO      300-SCREEN-EXT
      *
      *    請求確認画面
           WHEN    "I04"
               IF    ( SPA-I01-I04-BTN =   "B12" )
      *            画面を更新後の内容で再表示
                   PERFORM 4100-PTNUM-CHK-SEC
               ELSE
                   IF    ( SPA-I01-I04-PROCKBN =   "1" )
      *                カーソルを退院日に設定
                       MOVE    11          TO  SPA-GMN-I01-CUR
                   ELSE
      *                カーソルを選択番号に設定
                       MOVE    100         TO  SPA-GMN-I01-CUR
                   END-IF
               END-IF
      *
               GO  TO      300-SCREEN-EXT
      *
      *    オーダー画面
           WHEN    "I06"
               MOVE    SPA-I0-MOTOPG       TO  WRK-MOTOPG
               PERFORM 310-SPASET-SEC
               MOVE    WRK-MOTOPG          TO  SPA-I0-MOTOPG
               GO  TO      300-SCREEN-EXT
      *
      *    収納登録・入院会計照会・出産一時金画面
           WHEN    "S02"
           WHEN    "I41"
      *
      *        退避しておいたＳＰＡの内容を元に戻す
               INITIALIZE                  I0SUB01-LNK
               MOVE    SPA-TERMID      TO  I0SUB01-TERMID
               MOVE    "I"             TO  I0SUB01-KBN
               CALL    "ORCGI0SUB01"   USING   I0SUB01-LNK
                                               SPA-AREA
      *
               MOVE    I0SUB01-SCRSPA-AREA
                                       TO  SPA-I0FREE
               MOVE    I0SUB01-COMMON  TO  SPA-I0KYOUTU
      *
      *        療養病棟で特定入院料算定時に、上限日数超えで療養病棟入院料の
      *        算定が行われていない場合の注意メッセージ表示
               PERFORM   4101-RYOYO-CHK-SEC
      *
               GO  TO  300-SCREEN-EXT
           WHEN    "S08"
               INITIALIZE                  I0SUB01-LNK
               MOVE    SPA-TERMID      TO  I0SUB01-TERMID
               MOVE    "I"             TO  I0SUB01-KBN
               CALL    "ORCGI0SUB01"   USING   I0SUB01-LNK
                                               SPA-AREA
      *
               MOVE    I0SUB01-SCRSPA-AREA
                                       TO  SPA-I0FREE
               MOVE    I0SUB01-COMMON  TO  SPA-I0KYOUTU
      *
               IF    ( SPA-I0-MOTOPG2  =   "I01" )
      *
                   PERFORM 41011-BUNBEN-AREA-EDIT-SEC                   
      *
                   GO  TO  300-SCREEN-EXT
               ELSE
                   MOVE    SPA-PTNUM       TO  SPA-PTNUM
                   MOVE    SPA-PTID        TO  SPA-PTID
                   PERFORM 310-SPASET-SEC
                   MOVE    SPA-PTNUM       TO  I01-PTNUM
                   PERFORM 4100-PTNUM-CHK-SEC
                   GO  TO  300-SCREEN-EXT
               END-IF
      *    患者登録画面
           WHEN    "P02"
      *        退避しておいたＳＰＡの内容を元に戻す
               INITIALIZE                  I0SUB01-LNK
               MOVE    SPA-TERMID      TO  I0SUB01-TERMID
               MOVE    "I"             TO  I0SUB01-KBN
               CALL    "ORCGI0SUB01"   USING   I0SUB01-LNK
                                               SPA-AREA
      *
               MOVE    I0SUB01-SCRSPA-AREA
                                       TO  SPA-I0FREE
               MOVE    I0SUB01-COMMON  TO  SPA-I0KYOUTU
      *
               PERFORM 330-P02-SET-SEC
      *
               GO  TO  300-SCREEN-EXT
      *
      *    入院患者照会画面
           WHEN    "I20"
      *    診療行為画面
           WHEN    "K02N"
      *
               PERFORM 310-SPASET-SEC
               MOVE    SPA-MOTOPG      TO  SPA-I0-MOTOPG
      *
               MOVE    SPA-PTID        TO  SPA-NAI-I01-PTID
               MOVE    SPA-PTNUM       TO  SPA-GMN-I01-PTNUM
               PERFORM 4101-PTINF-SELECT-SEC
               IF    ( SPA-ERRCD   =   SPACE )
                   MOVE    "04"        TO  WRK-CMB-CD
                   PERFORM 4103-CMB-SYORIKBN-CHK-SEC
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SYORIKBN
               END-IF
      *
               INITIALIZE                  SPA-ERRCD
                                           SPA-I0IDCD
                                           SPA-I0IDCD2
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *    入外区分
           MOVE    "1"                 TO  SPA-NYUGAIKBN
      *
      *
           INITIALIZE                      SPA-I0FREE
                                           SPA-I0KYOUTU
      *
           INITIALIZE                      SYS-9000-REC
           MOVE    "9000"              TO  WRK-KANRICD
           MOVE    "*"                 TO  WRK-KBNCD
           MOVE    SPA-SYSYMD          TO  WRK-YUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC         TO  SYS-9000-REC
      *
           MOVE    SYS-9000-NYU-ORDER-FLG
                                       TO  SPA-GMN-I01-ORDERKBN
      *
      *    コンボボックス初期化
           MOVE    SPA-SYSYMD      TO  WRK-KJNYMD
           PERFORM 310-CMB-SYOKI-SEC
      *
      *    処理区分コンボ（全て表示）
           PERFORM 310-CMB-SYORIKBN-SET1-SEC
      *
      *    画面初期化
           PERFORM 420-CLEAR-SEC
      *
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       310-CMB-SYOKI-SEC               SECTION.
      *
      *    INITIALIZE              SPA-GMN-I01-CMB-DAT-AREA
      *
      *    室料差額コンボ
           PERFORM 310-CMB-SAGAKU-SET-SEC
      *
      *    食事区分コンボ
           PERFORM 310-CMB-SKJKBN-SET-SEC
      *
      *    食事時間区分コンボ
           PERFORM 310-CMB-TIMEKBN-SET-SEC
      *
      *    選定入院区分コンボ
           PERFORM 310-CMB-SENTEIKBN-SET-SEC
      *
      *    入院科コンボ
           PERFORM 310-CMB-NYUINKA-SET-SEC
      *
      *    退院事由コンボ
           PERFORM 310-CMB-TAIINCD-SET-SEC
      *
      *    初回コンボ
           PERFORM 310-CMB-SHOKAI-SET-SEC
      *
      *    介護異動コンボ
           PERFORM 310-CMB-KAIGO-SET-SEC
      *
      *    担当医コンボ
           PERFORM 310-CMB-DR-SET-SEC
      *
      *    定期請求コンボ
           PERFORM 310-CMB-SEIKYU-SET-SEC
      *
      *    検索時患者表示コンボ
           PERFORM 310-CMB-DISPKBN-SET-SEC
      *
      *    ユーザープログラムコンボ
           PERFORM 310-CMB-USERPG-SET-SEC
      *
      *    入院料算定コンボ
           PERFORM 310-CMB-NYUCALKBN-SET-SEC
      *
      *    分娩区分コンボ
           PERFORM 310-CMB-BUNBEN-SET-SEC
      *
      *    直接支払制度利用区分コンボ
           PERFORM 310-CMB-RIYOU-SET-SEC
      *
           .
       310-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボ初期処理（全て表示）
      *****************************************************************
       310-CMB-SYORIKBN-SET1-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-SYORIKBN
      *
           MOVE    11              TO  SPA-GMN-I01-SYORIKBN-CNT
      *
           MOVE    CONST-NYUIN-G       TO  SPA-GMN-I01-SYORIKBN-ITM (01)
           MOVE    CONST-TAIIN-G       TO  SPA-GMN-I01-SYORIKBN-ITM (02)
           MOVE    CONST-HENKOU-G      TO  SPA-GMN-I01-SYORIKBN-ITM (03)
           MOVE    CONST-SYOUKAI-G     TO  SPA-GMN-I01-SYORIKBN-ITM (04)
           MOVE    CONST-NYUINTORIKESHI-G
                                       TO  SPA-GMN-I01-SYORIKBN-ITM (05)
           MOVE    CONST-NYUINTORIKESHIALL-G
                                       TO  SPA-GMN-I01-SYORIKBN-ITM (06)
           MOVE    CONST-TAIINTORIKESHI-G
                                       TO  SPA-GMN-I01-SYORIKBN-ITM (07)
           MOVE    CONST-IDO-G         TO  SPA-GMN-I01-SYORIKBN-ITM (08)
           MOVE    CONST-IDOTORIKESHI-G
                                       TO  SPA-GMN-I01-SYORIKBN-ITM (09)
           MOVE    CONST-SAIKEISAN-G   TO  SPA-GMN-I01-SYORIKBN-ITM (10)
           MOVE    CONST-TAIINSRYHZN-G TO  SPA-GMN-I01-SYORIKBN-ITM (11)
      *
           .
       310-CMB-SYORIKBN-SET1-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボ初期処理（初入院）
      *****************************************************************
       310-CMB-SYORIKBN-SET2-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-SYORIKBN
      *
           MOVE    1               TO  SPA-GMN-I01-SYORIKBN-CNT
      *
           MOVE    CONST-NYUIN-G   TO  SPA-GMN-I01-SYORIKBN-ITM (01)
      *
      *    退院履歴チェック
           PERFORM 3101-TAIINRRK-CHK-SEC
      *
           .
       310-CMB-SYORIKBN-SET2-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボ初期処理（入院中）
      *****************************************************************
       310-CMB-SYORIKBN-SET3-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-SYORIKBN
      *
           MOVE    ZERO            TO  FLG-ALT-OK
      *
           IF    ( SPA-NAI-I01-MAEIDOYMD
                               NOT =   SPACE )
               MOVE    SPA-NAI-I01-MAEIDOYMD
                                   TO  WRK-YMD
               MOVE    "2"             TO  WRK-ZOGENPTN
               MOVE     1              TO  WRK-ZOGEN
               PERFORM 5002-CALENDAR-SEC
      D        DISPLAY "A MONTH LATER =" WRK-KEISANYMD
               IF    ( WRK-KEISANYMD   >   SPA-SYSYMD )
                   MOVE    1       TO  FLG-ALT-OK
               END-IF
      *
               IF    ( FLG-ALT-OK  =   1 )
                   PERFORM 900-PTNYUINRRK-KEY75-SEL-SEC
                   IF    ( FLG-PTNYUINRRK  =   ZERO )
                       MOVE    ZERO    TO  FLG-ALT-OK
                   END-IF
               END-IF
           END-IF
      *
           IF    ( FLG-ALT-OK      =   ZERO )
               MOVE    7               TO  SPA-GMN-I01-SYORIKBN-CNT
      *
               MOVE    CONST-TAIIN-G   TO  SPA-GMN-I01-SYORIKBN-ITM (01)
               MOVE    CONST-HENKOU-G  TO  SPA-GMN-I01-SYORIKBN-ITM (02)
               MOVE    CONST-SYOUKAI-G TO  SPA-GMN-I01-SYORIKBN-ITM (03)
               MOVE    CONST-NYUINTORIKESHI-G
                                       TO  SPA-GMN-I01-SYORIKBN-ITM (04)
               MOVE    CONST-NYUINTORIKESHIALL-G
                                       TO  SPA-GMN-I01-SYORIKBN-ITM (05)
               MOVE    CONST-IDO-G     TO  SPA-GMN-I01-SYORIKBN-ITM (06)
               MOVE    CONST-TAIINSRYHZN-G
                                       TO  SPA-GMN-I01-SYORIKBN-ITM (07)
           ELSE
               MOVE    8               TO  SPA-GMN-I01-SYORIKBN-CNT
      *
               MOVE    CONST-NYUINALT-G
                                   TO  SPA-GMN-I01-SYORIKBN-ITM (01)
               MOVE    CONST-TAIIN-G   TO  SPA-GMN-I01-SYORIKBN-ITM (02)
               MOVE    CONST-HENKOU-G  TO  SPA-GMN-I01-SYORIKBN-ITM (03)
               MOVE    CONST-SYOUKAI-G TO  SPA-GMN-I01-SYORIKBN-ITM (04)
               MOVE    CONST-NYUINTORIKESHI-G
                                       TO  SPA-GMN-I01-SYORIKBN-ITM (05)
               MOVE    CONST-NYUINTORIKESHIALL-G
                                       TO  SPA-GMN-I01-SYORIKBN-ITM (06)
               MOVE    CONST-IDO-G     TO  SPA-GMN-I01-SYORIKBN-ITM (07)
               MOVE    CONST-TAIINSRYHZN-G
                                       TO  SPA-GMN-I01-SYORIKBN-ITM (08)
           END-IF
      *
      *    退院履歴チェック
           PERFORM 3101-TAIINRRK-CHK-SEC
      *
           .
       310-CMB-SYORIKBN-SET3-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボ初期処理（入院歴あり−退院済）
      *****************************************************************
       310-CMB-SYORIKBN-SET4-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-SYORIKBN
      *
           MOVE    3               TO  SPA-GMN-I01-SYORIKBN-CNT
      *
           MOVE    CONST-NYUIN-G   TO  SPA-GMN-I01-SYORIKBN-ITM (01)
           MOVE    CONST-SYOUKAI-G TO  SPA-GMN-I01-SYORIKBN-ITM (02)
           MOVE    CONST-TAIINTORIKESHI-G
                                   TO  SPA-GMN-I01-SYORIKBN-ITM (03)
      *
      *    退院履歴チェック
           PERFORM 3101-TAIINRRK-CHK-SEC
      *
           .
       310-CMB-SYORIKBN-SET4-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボ初期処理（前回入院歴作成）
      *****************************************************************
       310-CMB-SYORIKBN-SET5-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-SYORIKBN
      *
           MOVE    2               TO  SPA-GMN-I01-SYORIKBN-CNT
      *
           MOVE    CONST-NYUIN-G   TO  SPA-GMN-I01-SYORIKBN-ITM (01)
           MOVE    CONST-SYOUKAI-G TO  SPA-GMN-I01-SYORIKBN-ITM (02)
      *
      *    退院履歴チェック
           PERFORM 3101-TAIINRRK-CHK-SEC
      *
      *
           .
       310-CMB-SYORIKBN-SET5-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボ初期処理（入院中）
      *****************************************************************
       310-CMB-SYORIKBN-SET6-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-SYORIKBN
      *
           MOVE    8               TO  SPA-GMN-I01-SYORIKBN-CNT
      *
           MOVE    CONST-TAIIN-G   TO  SPA-GMN-I01-SYORIKBN-ITM (01)
           MOVE    CONST-HENKOU-G  TO  SPA-GMN-I01-SYORIKBN-ITM (02)
           MOVE    CONST-SYOUKAI-G TO  SPA-GMN-I01-SYORIKBN-ITM (03)
           MOVE    CONST-NYUINTORIKESHI-G
                                   TO  SPA-GMN-I01-SYORIKBN-ITM (04)
           MOVE    CONST-NYUINTORIKESHIALL-G
                                   TO  SPA-GMN-I01-SYORIKBN-ITM (05)
           MOVE    CONST-IDO-G     TO  SPA-GMN-I01-SYORIKBN-ITM (06)
           MOVE    CONST-IDOTORIKESHI-G
                                   TO  SPA-GMN-I01-SYORIKBN-ITM (07)
           MOVE    CONST-TAIINSRYHZN-G
                                   TO  SPA-GMN-I01-SYORIKBN-ITM (08)
      *
      *    退院履歴チェック
           PERFORM 3101-TAIINRRK-CHK-SEC
      *
           .
       310-CMB-SYORIKBN-SET6-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボ初期処理（ロック中）
      *****************************************************************
       310-CMB-SYORIKBN-SET7-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-SYORIKBN
      *
           MOVE    1               TO  SPA-GMN-I01-SYORIKBN-CNT
      *
           MOVE    CONST-SYOUKAI-G TO  SPA-GMN-I01-SYORIKBN-ITM (01)
      *
           .
       310-CMB-SYORIKBN-SET7-EXT.
           EXIT.
      *****************************************************************
      *    退院履歴チェック処理
      *****************************************************************
       3101-TAIINRRK-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TAIINRRK
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-I01-NYUREKILST-CNT )
                    OR   ( FLG-TAIINRRK    NOT =   ZERO )
      *
               IF    ( SPA-NAI-I01-TZITA-FLG (IDX0)      =   ZERO )
                AND  ( SPA-NAI-I01-TTAIINYMD (IDX0)  NOT =   SPACE )
                   MOVE    1           TO  FLG-TAIINRRK
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-TAIINRRK    NOT =   ZERO )
               COMPUTE SPA-GMN-I01-SYORIKBN-CNT
                   =   SPA-GMN-I01-SYORIKBN-CNT
                   +   1
               MOVE    CONST-SAIKEISAN-G
                                       TO  SPA-GMN-I01-SYORIKBN-ITM
                                            (SPA-GMN-I01-SYORIKBN-CNT)
           END-IF
      *
           .
       3101-TAIINRRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額コンボ初期処理
      *****************************************************************
       310-CMB-SAGAKU-SET-SEC          SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-SAGAKU
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5113"          TO  SYS-KANRICD
           MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5113-REC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1            >   99 )
                   OR    ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    IDX1    TO  SPA-GMN-I01-SAGAKU-CNT
               MOVE    SYS-5113-KBNCD (1 : 2)
                           TO  SPA-GMN-I01-SAGAKUL (IDX1)
               MOVE    SYS-5113-BRM-SAGAKU-NM
                               TO  WRK-SAGAKU-NUM
               MOVE    "円"    TO  WRK-SAGAKU-EN
               MOVE    WRK-SAGAKU
                           TO  SPA-GMN-I01-SAGAKUMEIL(IDX1)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5113-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       310-CMB-SAGAKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事区分コンボ初期処理
      *****************************************************************
       310-CMB-SKJKBN-SET-SEC          SECTION.
      *
           MOVE    "5000"          TO  WRK-KANRICD
           MOVE    "*"             TO  WRK-KBNCD
           MOVE    WRK-KJNYMD      TO  WRK-YUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5000-REC
      *
           MOVE    SYS-5000-SKJKBN TO  SPA-NAI-I01-SKJKBN-DEF
      *
           INITIALIZE              SPA-GMN-I01-CMB-SKJKBN
                                   SPA-GMN-I01-CMB-TSKJKBN
      *
           IF    ( SPA-NAI-I01-PTRRKNUM    =   ZERO )
            OR  (( SPA-GMN-I01-SYORIKBN-CD =   "01" )
             AND ( SPA-NAI-I01-MAEIDOYMD   =   SPACE ))
               CONTINUE
           ELSE
               IF    ( SPA-NAI-I01-MAESKJKBN   =   "B" OR "C" )
                   CONTINUE
               ELSE
                   MOVE    "A"         TO  WRK-CMB-CD
                   MOVE    "変更無し"  TO  WRK-CMB-ITM
                   PERFORM 3101-CMB-SKJKBN-SET-SEC
               END-IF
           END-IF
      *
           MOVE    "0"             TO  WRK-CMB-CD
           MOVE    "食なし"        TO  WRK-CMB-ITM
           PERFORM 3101-CMB-SKJKBN-SET-SEC
      *
           MOVE    "1"             TO  WRK-CMB-CD
           MOVE    "食あり"        TO  WRK-CMB-ITM
           PERFORM 3101-CMB-SKJKBN-SET-SEC
      *
           IF     WRK-KJNYMD       <=  "20160331"
               IF    ( SYS-5000-SHOKUJIRYOYOCD
                                   =   CONST-SKJRYOYO2 )
                   CONTINUE
               ELSE
                   MOVE    "2"     TO  WRK-CMB-CD
                   MOVE    "特食"  TO  WRK-CMB-ITM
                   PERFORM 3101-CMB-SKJKBN-SET-SEC
               END-IF
           ELSE
               IF    ( SYS-5000-SHOKUJIRYOYOCD
                                   =   CONST-SKJRYOYO2 )
                   MOVE    "3"         TO  WRK-CMB-CD
                   MOVE    "流動食"    TO  WRK-CMB-ITM
                   PERFORM 3101-CMB-SKJKBN-SET-SEC
               ELSE
      *
                   MOVE    "2"         TO  WRK-CMB-CD
                   MOVE    "特食"      TO  WRK-CMB-ITM
                   PERFORM 3101-CMB-SKJKBN-SET-SEC
      *
                   MOVE    "3"         TO  WRK-CMB-CD
                   MOVE    "流動食"    TO  WRK-CMB-ITM
                   PERFORM 3101-CMB-SKJKBN-SET-SEC
      *
               END-IF
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-I01-SKJKBN-CNT )
      *
               IF    ( SPA-GMN-I01-SKJKBNL (IDX0) =   "A" )
                   CONTINUE
               ELSE
                   COMPUTE SPA-GMN-I01-TSKJKBN-CNT
                       =   SPA-GMN-I01-TSKJKBN-CNT     +   1
                   MOVE    SPA-GMN-I01-SKJKBN-ITM (IDX0)
                                   TO  SPA-GMN-I01-TSKJKBN-ITM
                                            (SPA-GMN-I01-TSKJKBN-CNT)
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-NAI-I01-MAESKJKBN   =   "B" OR "C" )
               MOVE    "B"         TO  WRK-CMB-CD
               MOVE    "変更無し"  TO  WRK-CMB-ITM
               PERFORM 3101-CMB-SKJKBN-SET-SEC
           END-IF
      *
           MOVE    "C"             TO  WRK-CMB-CD
           MOVE    "個別指示"      TO  WRK-CMB-ITM
           PERFORM 3101-CMB-SKJKBN-SET-SEC
      *
           .
       310-CMB-SKJKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事区分コンボ初期処理
      *****************************************************************
       3101-CMB-SKJKBN-SET-SEC          SECTION.
      *
           COMPUTE SPA-GMN-I01-SKJKBN-CNT
               =   SPA-GMN-I01-SKJKBN-CNT
               +   1
      *
           MOVE    WRK-CMB-CD
                       TO SPA-GMN-I01-SKJKBNL   (SPA-GMN-I01-SKJKBN-CNT)
      *
           MOVE    WRK-CMB-ITM
                       TO SPA-GMN-I01-SKJKBNMEIL(SPA-GMN-I01-SKJKBN-CNT)
      *
           .
       3101-CMB-SKJKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事時間区分コンボ初期処理
      *****************************************************************
       310-CMB-TIMEKBN-SET-SEC          SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-TIMEKBN
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "01" )
             AND ( SPA-NAI-I01-MAEIDOYMD   NOT =   SPACE )
               PERFORM 3102-CMB-TIMEKBN-SET-SEC
           ELSE
               PERFORM 3101-CMB-TIMEKBN-SET-SEC
           END-IF
      *
           .
       310-CMB-TIMEKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事時間区分コンボ初期処理
      *****************************************************************
       3101-CMB-TIMEKBN-SET-SEC          SECTION.
      *
           MOVE    SPA-NAI-I01-TTAIINYMD (1)   TO WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "01" )
            AND  ( SPA-NAI-I01-PTID        NOT =   ZERO )
            AND  ( WRK-SYMD                NOT =   SPACE )
            AND  ( SPA-NAI-I01-NYUINYMD
                                       <=  SPA-NAI-I01-TTAIINYMD (1) )
      *
               MOVE    ZERO            TO  FLG-LASTSKJ
      *
               PERFORM 900-NYUINACCT-KEY17-SEL-SEC
      *
               PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
                   IF    ( NACCT-DAY (WRK-SDD)     >   ZERO )
                       EVALUATE    NACCT-SRYCDTOTAL
                       WHEN    099999918
                           MOVE    1   TO  FLG-LASTSKJ
                       WHEN    099999919
                           MOVE    2   TO  FLG-LASTSKJ
                       WHEN    099999920
                           MOVE    3   TO  FLG-LASTSKJ
                       END-EVALUATE
                   END-IF
      *
                   PERFORM 900-NYUINACCT-KEY17-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               EVALUATE    FLG-LASTSKJ
               WHEN    1
                   MOVE    "0"         TO  SPA-GMN-I01-TIMEKBNL (1)
                   MOVE    "食事なし"  TO  SPA-GMN-I01-TIMEKBNMEIL (1)
      *
                   MOVE    "2"         TO  SPA-GMN-I01-TIMEKBNL (2)
                   MOVE    "昼食から"  TO  SPA-GMN-I01-TIMEKBNMEIL (2)
      *
                   MOVE    "3"         TO  SPA-GMN-I01-TIMEKBNL (3)
                   MOVE    "夕食から"  TO  SPA-GMN-I01-TIMEKBNMEIL (3)
      *
                   MOVE    3           TO  SPA-GMN-I01-TIMEKBN-CNT
      *
               WHEN    2
                   MOVE    "0"         TO  SPA-GMN-I01-TIMEKBNL (1)
                   MOVE    "食事なし"  TO  SPA-GMN-I01-TIMEKBNMEIL (1)
      *
                   MOVE    "3"         TO  SPA-GMN-I01-TIMEKBNL (2)
                   MOVE    "夕食から"  TO  SPA-GMN-I01-TIMEKBNMEIL (2)
      *
                   MOVE    2           TO  SPA-GMN-I01-TIMEKBN-CNT
      *
               WHEN    3
                   MOVE    "0"         TO  SPA-GMN-I01-TIMEKBNL (1)
                   MOVE    "食事なし"  TO  SPA-GMN-I01-TIMEKBNMEIL (1)
      *
                   MOVE    1           TO  SPA-GMN-I01-TIMEKBN-CNT
      *
               WHEN    OTHER
                   MOVE    "0"         TO  SPA-GMN-I01-TIMEKBNL (1)
                   MOVE    "食事なし"  TO  SPA-GMN-I01-TIMEKBNMEIL (1)
      *
                   MOVE    "1"         TO  SPA-GMN-I01-TIMEKBNL (2)
                   MOVE    "朝食から"  TO  SPA-GMN-I01-TIMEKBNMEIL (2)
      *
                   MOVE    "2"         TO  SPA-GMN-I01-TIMEKBNL (3)
                   MOVE    "昼食から"  TO  SPA-GMN-I01-TIMEKBNMEIL (3)
      *
                   MOVE    "3"         TO  SPA-GMN-I01-TIMEKBNL (4)
                   MOVE    "夕食から"  TO  SPA-GMN-I01-TIMEKBNMEIL (4)
      *
                   MOVE    4           TO  SPA-GMN-I01-TIMEKBN-CNT
      *
               END-EVALUATE
      *
           ELSE
               MOVE    "0"         TO  SPA-GMN-I01-TIMEKBNL (1)
               MOVE    "食事なし"  TO  SPA-GMN-I01-TIMEKBNMEIL (1)
      *
               MOVE    "1"         TO  SPA-GMN-I01-TIMEKBNL (2)
               MOVE    "朝食から"    TO  SPA-GMN-I01-TIMEKBNMEIL (2)
      *
               MOVE    "2"         TO  SPA-GMN-I01-TIMEKBNL (3)
               MOVE    "昼食から"    TO  SPA-GMN-I01-TIMEKBNMEIL (3)
      *
               MOVE    "3"         TO  SPA-GMN-I01-TIMEKBNL (4)
               MOVE    "夕食から"    TO  SPA-GMN-I01-TIMEKBNMEIL (4)
      *
               MOVE    4           TO  SPA-GMN-I01-TIMEKBN-CNT
      *
           END-IF
      *
           .
       3101-CMB-TIMEKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事時間区分コンボ初期処理
      *****************************************************************
       3102-CMB-TIMEKBN-SET-SEC        SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-TIMEKBN
      *
           MOVE    ZERO        TO  FLG-ASA
                                   FLG-HIRU
                                   FLG-YU
      *
           MOVE    SPACE       TO  WRK-TIMEKBN-PATTERN
      *
           MOVE    SPA-NAI-I01-NYUINYMD
                               TO WRK-YMD
      *
           PERFORM 900-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               IF    ( NACCT-DAY (WRK-SDD)     >   ZERO )
                   EVALUATE    NACCT-SRYCDTOTAL
                   WHEN    099999918
                       MOVE    1   TO  FLG-ASA
                   WHEN    099999919
                       MOVE    2   TO  FLG-HIRU
                   WHEN    099999920
                       MOVE    3   TO  FLG-YU
                   END-EVALUATE
               END-IF
      *
               PERFORM 900-NYUINACCT-KEY17-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           EVALUATE    SPA-GMN-I01-TIMEKBN-CD
           WHEN    "1"
               MOVE    ZERO        TO  WRK-TIMEKBN-PATTERN
           WHEN    "2"
               IF    ( FLG-ASA     NOT =   ZERO )
                   MOVE    "1"     TO  WRK-TIMEKBN-PATTERN
               ELSE
                   MOVE    ZERO    TO  WRK-TIMEKBN-PATTERN
               END-IF
           WHEN   "3"
               EVALUATE    TRUE
               WHEN  ( FLG-HIRU    NOT =   ZERO )
                   MOVE    "2"     TO  WRK-TIMEKBN-PATTERN
               WHEN  ( FLG-ASA     NOT =   ZERO )
                   MOVE    "1"     TO  WRK-TIMEKBN-PATTERN
               WHEN    OTHER
                   MOVE    ZERO    TO  WRK-TIMEKBN-PATTERN
               END-EVALUATE
           WHEN   OTHER
               EVALUATE    TRUE
               WHEN  ( FLG-YU      NOT =   ZERO )
                   MOVE    "3"     TO  WRK-TIMEKBN-PATTERN
               WHEN  ( FLG-HIRU    NOT =   ZERO )
                   MOVE    "2"     TO  WRK-TIMEKBN-PATTERN
               WHEN  ( FLG-ASA     NOT =   ZERO )
                   MOVE    "1"     TO  WRK-TIMEKBN-PATTERN
               WHEN    OTHER
                   MOVE    ZERO    TO  WRK-TIMEKBN-PATTERN
               END-EVALUATE
           END-EVALUATE
      *
           MOVE    ZERO        TO  IDXL
      *
           COMPUTE IDXL  = IDXL    +   1
      *
           MOVE    "0"         TO  SPA-GMN-I01-TIMEKBNL (IDXL)
           MOVE    "食事なし"  TO  SPA-GMN-I01-TIMEKBNMEIL (IDXL)
      *
      D    DISPLAY "WRK-TIMEKBN-PATTERN = "  WRK-TIMEKBN-PATTERN
           IF    ( WRK-TIMEKBN-PATTERN <   "1")
               COMPUTE IDXL  = IDXL    +   1
               MOVE    "1"         TO  SPA-GMN-I01-TIMEKBNL (IDXL)
               MOVE    "朝食から"  TO  SPA-GMN-I01-TIMEKBNMEIL (IDXL)
           END-IF
      *
           IF    ( WRK-TIMEKBN-PATTERN <   "2" )
               COMPUTE IDXL  = IDXL    +   1
               MOVE    "2"         TO  SPA-GMN-I01-TIMEKBNL (IDXL)
               MOVE    "昼食から"  TO  SPA-GMN-I01-TIMEKBNMEIL (IDXL)
           END-IF
      *
           IF    ( WRK-TIMEKBN-PATTERN <   "3" )
               COMPUTE IDXL  = IDXL    +   1
               MOVE    "3"         TO  SPA-GMN-I01-TIMEKBNL (IDXL)
               MOVE    "夕食から"  TO  SPA-GMN-I01-TIMEKBNMEIL (IDXL)
           END-IF
      *
           MOVE    IDXL            TO  SPA-GMN-I01-TIMEKBN-CNT
      *
           .
      *
       3102-CMB-TIMEKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    選定入院区分コンボ初期処理
      *****************************************************************
       310-CMB-SENTEIKBN-SET-SEC          SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-SENTEIKBN
           MOVE    2               TO  SPA-GMN-I01-SENTEIKBN-CNT
      *
           MOVE    "1"             TO  SPA-GMN-I01-SENTEIKBNL (1)
           MOVE    "選定対象"      TO  SPA-GMN-I01-SENTEIKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-I01-SENTEIKBNL (2)
           MOVE    "選定対象外"    TO  SPA-GMN-I01-SENTEIKBNMEIL (2)
      *
           .
       310-CMB-SENTEIKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院科コンボ初期処理
      *****************************************************************
       310-CMB-NYUINKA-SET-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-NYUINKA
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1005"          TO  SYS-KANRICD
           MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1            >   99 )
                   OR    ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    IDX1    TO  SPA-GMN-I01-NYUINKA-CNT
               MOVE    SYS-1005-KBNCD (1 : 2)
                           TO  SPA-GMN-I01-NYUINKAL (IDX1)
               MOVE    SYS-1005-SRYKANAME
                           TO  SPA-GMN-I01-NYUINKAMEIL (IDX1)
               MOVE    SYS-1005-RECESRYKA
                           TO  SPA-GMN-I01-NYUINKA-ITM2(IDX1)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       310-CMB-NYUINKA-SET-EXT.
           EXIT.
      *****************************************************************
      *    退院事由コンボ初期処理
      *****************************************************************
       310-CMB-TAIINCD-SET-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-TAIINCD
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5013"          TO  SYS-KANRICD
           MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5013-REC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1            >   50 )
                   OR    ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    IDX1    TO  SPA-GMN-I01-TAIINCD-CNT
               MOVE    SYS-5013-KBNCD (1 : 2)
                           TO  SPA-GMN-I01-TAIINCDL    (IDX1)
               MOVE    SYS-5013-NAME
                           TO  SPA-GMN-I01-TAIINCDMEIL (IDX1)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC TO SYS-5013-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       310-CMB-TAIINCD-SET-EXT.
           EXIT.
      *****************************************************************
      *    初回コンボ初期処理
      *****************************************************************
       310-CMB-SHOKAI-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-SHOKAI
      *
           MOVE    2                   TO  SPA-GMN-I01-SHOKAI-CNT
      *
           MOVE    "1"                 TO  SPA-GMN-I01-SHOKAIL (01)
           MOVE    "初回"              TO  SPA-GMN-I01-SHOKAIMEIL (01)
      *
           MOVE    "2"                 TO  SPA-GMN-I01-SHOKAIL (02)
           MOVE    "継続"              TO  SPA-GMN-I01-SHOKAIMEIL (02)
      *
           .
       310-CMB-SHOKAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    介護異動コンボ初期処理
      *****************************************************************
       310-CMB-KAIGO-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-KAIGO
      *
           MOVE    2                   TO  SPA-GMN-I01-KAIGO-CNT
      *
           MOVE    "1"                 TO  SPA-GMN-I01-KAIGOL (01)
           MOVE    "対象外"            TO  SPA-GMN-I01-KAIGOMEIL (01)
      *
           MOVE    "2"                 TO  SPA-GMN-I01-KAIGOL (02)
           MOVE    "急性増悪により"    TO  SPA-GMN-I01-KAIGOMEIL (02)
      *
           .
       310-CMB-KAIGO-SET-EXT.
           EXIT.
      *****************************************************************
      *    担当医コンボ初期処理
      *****************************************************************
       310-CMB-DR-SET-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-DR
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1010"          TO  SYS-KANRICD
           MOVE    "1%"            TO  SYS-KBNCD
           MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY9-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1010-REC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1            >   99 )
                   OR    ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    IDX1    TO  SPA-GMN-I01-DR-CNT
               MOVE    SYS-1010-KBNCD (2 : 4)
                           TO  SPA-GMN-I01-DRL (IDX1)
               MOVE    SYS-1010-NAME
                           TO  SPA-GMN-I01-DRMEIL(IDX1)
      *
               PERFORM 900-SYSKANRI-KEY9-FET-SEC
               MOVE    SYSKANRI-REC TO  SYS-1010-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       310-CMB-DR-SET-EXT.
           EXIT.
      *****************************************************************
      *    定期請求コンボ初期処理
      *****************************************************************
       310-CMB-SEIKYU-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-SEIKYU
      *
           MOVE    3               TO  SPA-GMN-I01-SEIKYU-CNT
      *
           MOVE    "1"             TO  SPA-GMN-I01-SEIKYUL (01)
           MOVE    "医療機関での設定"
                                   TO  SPA-GMN-I01-SEIKYUMEIL (01)
      *
           MOVE    "2"             TO  SPA-GMN-I01-SEIKYUL (02)
           MOVE    "月末時のみ請求"
                                   TO  SPA-GMN-I01-SEIKYUMEIL (02)
      *
           MOVE    "3"             TO  SPA-GMN-I01-SEIKYUL (03)
           MOVE    "定期請求しない"
                                   TO  SPA-GMN-I01-SEIKYUMEIL (03)
      *
      *
           .
       310-CMB-SEIKYU-SET-EXT.
           EXIT.
      *****************************************************************
      *    検索時患者表示コンボ初期処理
      *****************************************************************
       310-CMB-DISPKBN-SET-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-DISPKBN
      *
           MOVE    2               TO  SPA-GMN-I01-DISPKBN-CNT
      *
           MOVE    "1"             TO  SPA-GMN-I01-DISPKBNL    (1)
           MOVE    "表示可"        TO  SPA-GMN-I01-DISPKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-I01-DISPKBNL    (2)
           MOVE    "表示不可"      TO  SPA-GMN-I01-DISPKBNMEIL (2)
      *
           .
       310-CMB-DISPKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    ユーザープログラムコンボ初期処理
      *****************************************************************
       310-CMB-USERPG-SET-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-USERPG
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "02" OR "04" 
                                            OR "10" OR "11" )
               MOVE    ZERO        TO  SPA-GMN-I01-USERPG-CNT
               MOVE    SPACE       TO  SPA-GMN-I01-USERPG
           ELSE
      *
               MOVE    1           TO  SPA-GMN-I01-USERPG-CNT
      *
               MOVE    "0"         TO  SPA-GMN-I01-USERPGL    (1)
               MOVE    "Ｕ・Ｐ指示なし" 
                                   TO  SPA-GMN-I01-USERPGMEIL (1)
      *
               INITIALIZE              ORCSUSERCHKAREA
               MOVE    "I01"       TO  ORCSUSERCHK-GMNPG
               MOVE    "1"         TO  ORCSUSERCHK-SYORI
               CALL    "ORCSUSERCHK"   USING
                                       ORCSUSERCHKAREA
                                       SPA-AREA
               MOVE    ORCSUSERCHK-GYOUMU-FLG
                                   TO  SPA-NAI-I01-GYOUMU-FLG
               MOVE    ORCSUSERCHK-GYOUMU-ARI
                                   TO  SPA-NAI-I01-GYOUMU-ARI
      *
               IF    ( SPA-NAI-I01-GYOUMU-ARI  =   "1" )
      *
      *            実行するプログラムがある時
                   MOVE    2       TO  SPA-GMN-I01-USERPG-CNT
      *
                   MOVE    "1"     TO  SPA-GMN-I01-USERPGL  (2)
                   MOVE    "Ｕ・Ｐ指示あり" 
                                   TO  SPA-GMN-I01-USERPGMEIL (2)
      *
                   IF    ( SPA-NAI-I01-GYOUMU-FLG  =   "1" )
                       MOVE    SPA-GMN-I01-USERPG-ITM (2)
                                   TO  SPA-GMN-I01-USERPG
                   ELSE
                       MOVE    SPA-GMN-I01-USERPG-ITM (1)
                                   TO  SPA-GMN-I01-USERPG
                   END-IF
               ELSE
                   MOVE    SPA-GMN-I01-USERPG-ITM (1)
                                   TO  SPA-GMN-I01-USERPG
               END-IF
           END-IF
      *
           .
       310-CMB-USERPG-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院料算定コンボ初期処理
      *****************************************************************
       310-CMB-NYUCALKBN-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-NYUCALKBN
      *
           MOVE    2                   TO  SPA-GMN-I01-NYUCALKBN-CNT
      *
           MOVE    "1"                 TO  SPA-GMN-I01-NYUCALKBNL (01)
           MOVE    "入院料を算定しない"
                                       TO  SPA-GMN-I01-NYUCALKBNMEIL(01)
      *
           MOVE    "2"                 TO  SPA-GMN-I01-NYUCALKBNL (02)
           MOVE    "入院料を算定する"  TO  SPA-GMN-I01-NYUCALKBNMEIL(02)
      *
           .
       310-CMB-NYUCALKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    分娩コンボ初期処理
      *****************************************************************
       310-CMB-BUNBEN-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-BUNBEN
      *
           MOVE    3                   TO  SPA-GMN-I01-BUNBEN-CNT
      *
           MOVE    "0"                 TO  SPA-GMN-I01-BUNBENL (01)
           MOVE    "分娩入院でない"    TO  SPA-GMN-I01-BUNBENMEIL (01)
      *
           MOVE    "1"                 TO  SPA-GMN-I01-BUNBENL (02)
           MOVE    "正常分娩"          TO  SPA-GMN-I01-BUNBENMEIL (02)
      *
           MOVE    "2"                 TO  SPA-GMN-I01-BUNBENL (03)
           MOVE    "異常分娩"          TO  SPA-GMN-I01-BUNBENMEIL (03)
      *
           .
       310-CMB-BUNBEN-SET-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分コンボ初期処理
      *****************************************************************
       310-CMB-RIYOU-SET-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-RIYOU
      *
           MOVE    2                   TO  SPA-GMN-I01-RIYOU-CNT
      *
           MOVE    "0"                 TO  SPA-GMN-I01-RIYOUL (01)
           MOVE    "利用しない"        TO  SPA-GMN-I01-RIYOUMEIL (01)
      *
           MOVE    "1"                 TO  SPA-GMN-I01-RIYOUL (02)
           MOVE    "利用する"          TO  SPA-GMN-I01-RIYOUMEIL (02)
      *
           .
       310-CMB-RIYOU-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料非該当コンボボックス初期処理
      *****************************************************************
       310-CMB-TOKHIGAITO-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-TOKHIGAITO
      *
           MOVE    2               TO  SPA-GMN-I01-TOKHIGAITO-CNT
      *
           MOVE    "0"             TO  SPA-GMN-I01-TOKHIGAITOL (01)
           MOVE    "算定要件に該当する患者" 
                                   TO  SPA-GMN-I01-TOKHIGAITOMEIL (01)
      *
           MOVE    "1"             TO  SPA-GMN-I01-TOKHIGAITOL (02)
           MOVE    "算定要件に該当しない患者"
                                   TO  SPA-GMN-I01-TOKHIGAITOMEIL (02)
      *
           .
       310-CMB-TOKHIGAITO-SET-EXT.
           EXIT.
      *****************************************************************
      *    氏名検索画面（Ｐ９７）からの戻り値設定処理
      *****************************************************************
       330-P97-SET-SEC                 SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *
      *    人が選ばれなかった場合、
      *    同一の人が選択された場合は、画面内容の変更は行わない
           IF    ( WRK-SPA-PTID    =   ZERO )
             OR  ( WRK-SPA-PTID    =   SPA-NAI-I01-PTID )
               GO  TO  330-P97-SET-EXT
           END-IF
      *
      *    画面初期化
           PERFORM 420-CLEAR-SEC
      *
           MOVE    WRK-SPA-PTID        TO  SPA-NAI-I01-PTID 
           MOVE    WRK-SPA-PTNUM       TO  SPA-GMN-I01-PTNUM
      *
           PERFORM 4101-PTINF-SELECT-SEC
      *
           .
       330-P97-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録画面（Ｐ０２）からの戻り値設定処理
      *****************************************************************
       330-P02-SET-SEC                 SECTION.
      *
           IF    ( SPA-ERRMSG(1:2)     =   "NO" )
      *        戻る
               IF      SPA-PTNUM(1:1)     =   "*"
      *            画面初期化
                   PERFORM 420-CLEAR-SEC
               END-IF
               GO  TO  330-P02-SET-EXT
           END-IF
      *
           IF    ( SPA-PTID            =   ZERO )
      *        画面初期化
               PERFORM 420-CLEAR-SEC
               GO  TO  330-P02-SET-EXT
           END-IF
      *
           MOVE    SPA-PTID            TO  SPA-NAI-I01-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-I01-PTNUM
      *
           PERFORM 4101-PTINF-SELECT-SEC
      *
           .
       330-P02-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（Ｉ０ＩＤ１）からの戻り値設定処理
      *****************************************************************
       330-I0ID1-SET-SEC               SECTION.
      *
           MOVE    SPA-I0ID1-FLG   TO  WRK-I0ID-FLG
           MOVE    SPA-I0IDCD      TO  WRK-I0IDCD
      *
           MOVE    SPACE           TO  SPA-I0ID1-FLG
           MOVE    SPACE           TO  SPA-I0IDCD
      *
           IF    ( WRK-I0ID-FLG    =   "OK" )
               MOVE    SPACE       TO  WRK-UPDKBN
               DISPLAY "I0ID PTRRKNUM:" SPA-NAI-I01-PTRRKNUM
               EVALUATE    WRK-I0IDCD
               WHEN    "3009"
                   MOVE    1       TO  FLG-TAINTUSANMINUS
                   IF    ( SPA-NAI-I01-MAEIDOYMD   =   SPACE )
                       MOVE    "INS"   TO  WRK-UPDKBN
                   ELSE
                       MOVE    "UPD"   TO  WRK-UPDKBN
                   END-IF
                   PERFORM 4801-PTNYUINRRK-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  330-I0ID1-SET-EXT
                   END-IF
               WHEN    "3010"
                   IF    ( SPA-NAI-I01-MAEIDOYMD   =   SPACE )
                       MOVE    "INS"   TO  WRK-UPDKBN
                   ELSE
                       MOVE    "UPD"   TO  WRK-UPDKBN
                   END-IF
                   PERFORM 4801-PTNYUINRRK-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  330-I0ID1-SET-EXT
                   END-IF
      *        入院取消
               WHEN "3001"
               WHEN "3006"
      *        入院取消（会計含む）
               WHEN "3002"
               WHEN "3007"
                   MOVE    "DEL"   TO  WRK-UPDKBN
                   PERFORM 4801-PTNYUINRRK-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  330-I0ID1-SET-EXT
                   END-IF
      *        退院取消
               WHEN "3003"
                   MOVE    "UPD"   TO  WRK-UPDKBN
                   PERFORM 4801-PTNYUINRRK-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  330-I0ID1-SET-EXT
                   END-IF
      *        退院登録
               WHEN "3008"
                   MOVE    ZERO    TO  FLG-TEIKI-CHK
                   PERFORM 480-TOUROKU-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  330-I0ID1-SET-EXT
                   END-IF
               WHEN "3004"
               WHEN "3005"
               WHEN "3011"
                   IF    ( WRK-I0IDCD      =   "3005" )
      *
                       MOVE    1   TO  SPA-NAI-I01-TORIKESI-FLG
      *
      *                診療会計テーブルチェック処理
                       PERFORM 490-SRYACCT-CHK-SEC
                       IF    ( SPA-I0IDCD  NOT =   SPACE )
                         OR  ( SPA-ERRCD   NOT =   SPACE )
                           GO  TO  330-I0ID1-SET-EXT
                       END-IF
                   END-IF
      *
      *            請求開始日取得処理
                   PERFORM 4801-SKYSTYMD-GET-SEC
      *            請求確認画面遷移処理
                   MOVE    "JOIN"      TO  WRK-MCP-PUTTYPE
                   PERFORM 490-TAIIN-SYORI-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  330-I0ID1-SET-EXT
                   END-IF
      *
               END-EVALUATE
      *
           ELSE
               EVALUATE    WRK-I0IDCD
      *        入院取消
               WHEN "3001"
               WHEN "3006"
      *        入院取消（会計含む）
               WHEN "3002"
               WHEN "3007"
      *        退院取消
               WHEN "3003"
                   MOVE    02      TO  SPA-GMN-I01-CUR
      *        退院登録
               WHEN "3004"
               WHEN "3011"
                   MOVE    11      TO  SPA-GMN-I01-CUR
               END-EVALUATE
           END-IF
      *
           .
       330-I0ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（Ｉ０ＩＤ２）からの戻り値設定処理
      *****************************************************************
       330-I0ID2-SET-SEC               SECTION.
      *
           MOVE    SPA-I0ID2-FLG   TO  WRK-I0ID-FLG
           MOVE    SPA-I0IDCD2     TO  WRK-I0IDCD
      *
           MOVE    SPACE           TO  SPA-I0ID2-FLG
           MOVE    SPACE           TO  SPA-I0IDCD2
      *
           IF    ( WRK-I0ID-FLG    =   "OK" )
               MOVE    SPACE       TO  WRK-UPDKBN
               EVALUATE    WRK-I0IDCD
               WHEN "3101"
                   MOVE    "DEL"   TO  WRK-UPDKBN
                   PERFORM 4801-PTNYUINRRK-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  330-I0ID2-SET-EXT
                   END-IF
      *
               END-EVALUATE
           END-IF
      *
           .
       330-I0ID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院歴表示画面から戻った場合の処理
      *****************************************************************
       330-I02-SET-SEC                 SECTION.
      *
           IF    ( SPA-NAI-I01-PTRRKNUM    =   SPA-I01-I02-PTRRKNUM )
            AND  ( SPA-I01-I02-TAIINYMD    NOT =   SPACE )
               MOVE    SPA-I01-I02-TAIINCD TO  WRK-CMB-CD
               PERFORM 4103-CMB-TAIINCD-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TAIINCD
           END-IF
      *
           .
       330-I02-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院歴作成画面から戻った場合の処理
      *****************************************************************
       330-I03-SET-SEC                 SECTION.
      *
      *    戻るボタンで戻ってきた場合は何もしない
           IF    ( SPA-I01-I03-BTN =   "B01" )
               GO  TO  330-I03-SET-EXT
           END-IF
      *
      *    入院歴リストを再編集する
           PERFORM 4101-NYUREKI-EDIT-SEC
      *
      *    最新の入院歴を取得する
           PERFORM 4101-PTNYUINRRK-SEL1-SEC
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" )
            AND  ( SPA-NAI-I01-MAEIDOYMD   =   SPACE )
               CONTINUE
           ELSE
               MOVE    PTNYUINRRK-RRKNUM   TO  SPA-NAI-I01-PTRRKNUM
               MOVE    PTNYUINRRK-RRKEDANUM
                                           TO  SPA-NAI-I01-PTRRKEDANUM
           END-IF
      D    DISPLAY "FROM I03 PTRRKNUM:" SPA-NAI-I01-PTRRKNUM
      *
           IF  (   SPA-I01-I03-SYORIKBN        =   "2" OR  "3" )
               IF    (   FLG-PTNYUINRRK        =   ZERO )
                   IF    ( PTNYUINRRK-JTIKBN   NOT =   "5" AND "6" )
                    AND  ( PTNYUINRRK-TAIINYMD =   "99999999" )
                       MOVE    1       TO  SPA-GMN-I01-NYUREKILST-SEL
                                           SPA-GMN-I01-SELNUM
                       GO  TO  330-I03-SET-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF  (   SPA-I01-I03-SYORIKBN-CD     =   "01" )
      *        入院歴なし（初入院）
               IF  (   FLG-PTNYUINRRK  NOT =   ZERO )
                   PERFORM 310-CMB-SYORIKBN-SET2-SEC
                   MOVE    SPA-GMN-I01-SYORIKBN-ITM (01)
                                       TO  SPA-GMN-I01-SYORIKBN
               ELSE
      *            歴作成データ有り
                   IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
                       MOVE    "01"    TO  SPA-GMN-I01-SYORIKBN-CD
                       PERFORM 310-CMB-SYORIKBN-SET5-SEC
                       MOVE    SPA-GMN-I01-SYORIKBN-CD
                                           TO  WRK-CMB-CD
                       PERFORM 4103-CMB-SYORIKBN-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SYORIKBN
                   ELSE
                       MOVE    "01"    TO  SPA-GMN-I01-SYORIKBN-CD
                       PERFORM 310-CMB-SYORIKBN-SET4-SEC
                       MOVE    SPA-GMN-I01-SYORIKBN-CD
                                           TO  WRK-CMB-CD
                       PERFORM 4103-CMB-SYORIKBN-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SYORIKBN
                   END-IF
               END-IF
           ELSE
      *        入院歴なし（初入院）
               IF  (   FLG-PTNYUINRRK  NOT =   ZERO )
                   MOVE    SPA-SYSYMD  TO  WRK-KJNYMD
                   PERFORM 310-CMB-SYOKI-SEC
                   PERFORM 4201-INPUT-AREA-CLEAR-SEC
                   PERFORM 310-CMB-SYORIKBN-SET2-SEC
                   MOVE    SPACE       TO  SPA-GMN-I01-SYORIKBN
               ELSE
      *            歴作成データ有り
                   IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
                       MOVE    "02"    TO  SPA-GMN-I01-SYORIKBN-CD
                       PERFORM 4101-INPUT-AREA-EDIT2-SEC
                       PERFORM 310-CMB-SYORIKBN-SET5-SEC
                       MOVE    SPA-GMN-I01-SYORIKBN-CD
                                           TO  WRK-CMB-CD
                       PERFORM 4103-CMB-SYORIKBN-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SYORIKBN
                   ELSE
                       MOVE    "02"    TO  SPA-GMN-I01-SYORIKBN-CD
                       PERFORM 4101-INPUT-AREA-EDIT-SEC
                       PERFORM 310-CMB-SYORIKBN-SET4-SEC
                       MOVE    SPA-GMN-I01-SYORIKBN-CD
                                           TO  WRK-CMB-CD
                       PERFORM 4103-CMB-SYORIKBN-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SYORIKBN
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-NYUREKILST-CNT  >   ZERO )
               MOVE    1           TO  SPA-GMN-I01-NYUREKILST-SEL
                                       SPA-GMN-I01-SELNUM
           ELSE
               MOVE   ZERO         TO  SPA-GMN-I01-NYUREKILST-SEL
                                       SPA-GMN-I01-SELNUM
           END-IF
      *
           .
       330-I03-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 210-BACK
           WHEN    "CLICKED"       ALSO    "B02"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 420-CLEAR-SEC
           WHEN    "CLICKED"       ALSO    "B03"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 420-MAEPTINF-SET-SEC
           WHEN    "CLICKED"       ALSO    "B05"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-KANJATOROKU-SEC
           WHEN    "CLICKED"       ALSO    "B06"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-NYUINREKI-SEC
           WHEN    "CLICKED"       ALSO    "B07"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-NYUINRRK-SAKUSEI-SEC
           WHEN    "CLICKED"       ALSO    "B08"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-NYUINRRK-SYUSEI-SEC
           WHEN    "CLICKED"       ALSO    "B09"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-KENSAKU-SEC
           WHEN    "CLICKED"       ALSO    "B12"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               MOVE    1                   TO  FLG-TEIKI-CHK
               PERFORM 480-TOUROKU-SEC
           WHEN    "CLICKED"       ALSO    "B10"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-TAIIN-PRT-SEC
           WHEN    "CLICKED"       ALSO    "B11"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-KARUTE-PRT-SEC
           WHEN    "CLICKED"       ALSO    "B15"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-S08-SENI-SEC
           WHEN    "CLICKED"       ALSO    "B23"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-ZOKUSI-PRT-SEC
           WHEN    "CLICKED"       ALSO    "B20"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-S02-SENI-SEC
           WHEN    "CLICKED"       ALSO    "B21"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-NYUINKAIKEI-SEC
           WHEN    "CLICKED"       ALSO    "B24"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-I06-SENI-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    患者番号チェック
           WHEN    "ACTIVATE"      ALSO    "PTNUM"
               PERFORM 4100-PTNUM-CHK-SEC
      *    処理区分チェック
           WHEN    "ACTIVATE"      ALSO    "SYORIKBN"
               PERFORM 4101-PTNUM-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                 OR  ( FLG-END     NOT =   ZERO  )
                   CONTINUE
               ELSE
                   PERFORM 4100-SYORIKBN-CHK-SEC
               END-IF
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    入院歴リスト
           WHEN        ANY         ALSO    "NYUREKILST"
               PERFORM 4101-NYUREKILST-SEL-SEC
           WHEN        OTHER
               MOVE    ZERO        TO      SPA-GMN-I01-CUR
      *
           END-EVALUATE
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    患者番号チェック
           PERFORM 4101-PTNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
            OR   ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    処理区分チェック
           PERFORM 4101-SYORIKBN-CHK-SEC
           IF    ( SPA-ERRCD NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    入院退院登録画面で登録した入院歴のみチェックする
      *    取消処理もチェック対象外とする。
           IF    ( SPA-NAI-I01-NYURRK-FLG  NOT = ZERO )
            OR   ( SPA-GMN-I01-SYORIKBN-CD     =   "05" )
            OR   ( SPA-GMN-I01-SYORIKBN-CD     =   "06" )
            OR   ( SPA-GMN-I01-SYORIKBN-CD     =   "07" )
            OR   ( SPA-GMN-I01-SYORIKBN-CD     =   "09" )
               CONTINUE
           ELSE
      *        基本チェック処理
               PERFORM 4102-KIHON-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                 OR  ( FLG-END     NOT =   ZERO  )
                   GO  TO  410-INPUT-CHK-EXT
               END-IF
      *
           END-IF
      *
      *    選択番号チェック処理
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    SPA-GMN-I01-SKJKBN-CD
                                       TO  WRK-SKJKBN-MAE
      *
      *    異動日
           MOVE    I01-IDOYMD          TO  SPA-GMN-I01-IDOYMD
      *    病室番号
           MOVE    I01-BRMNUM          TO  SPA-GMN-I01-BRMNUM
      *    病棟名称
           MOVE    I01-BTUNAME         TO  SPA-GMN-I01-BTUNAME
      *    室料差額
           MOVE    I01-SAGAKU          TO  SPA-GMN-I01-SAGAKU
      *    食事区分
           MOVE    I01-SKJKBN          TO  SPA-GMN-I01-SKJKBN
      *    食事区分（個別指示）
           MOVE    I01-TSKJKBN (1)     TO  SPA-GMN-I01-TSKJKBN (1)
           MOVE    I01-TSKJKBN (2)     TO  SPA-GMN-I01-TSKJKBN (2)
           MOVE    I01-TSKJKBN (3)     TO  SPA-GMN-I01-TSKJKBN (3)
      *    食事時間区分
           MOVE    I01-TIMEKBN         TO  SPA-GMN-I01-TIMEKBN
      *    選定入院区分
           MOVE    I01-SENTEIKBN       TO  SPA-GMN-I01-SENTEIKBN
      *    入院日
           MOVE    I01-NYUINYMD        TO  SPA-GMN-I01-NYUINYMD
      *    入院科
           MOVE    I01-NYUINKA         TO  SPA-GMN-I01-NYUINKA
      *    初回
           MOVE    I01-SHOKAI          TO  SPA-GMN-I01-SHOKAI
      *    初回番号
           MOVE    I01-SHONUM          TO  SPA-GMN-I01-SHONUM
      *    初回入院日
           MOVE    I01-LBL-SHONYUINYMD TO  SPA-GMN-I01-LBL-SHONYUINYMD
      *    介護異動
           MOVE    I01-KAIGO           TO  SPA-GMN-I01-KAIGO
      *    退院日
           MOVE    I01-TAIINYMD        TO  SPA-GMN-I01-TAIINYMD
      *    退院事由
           MOVE    I01-TAIINCD         TO  SPA-GMN-I01-TAIINCD
      *    担当医
           MOVE    I01-DR (1)          TO  SPA-GMN-I01-DR (1)
           MOVE    I01-DR (2)          TO  SPA-GMN-I01-DR (2)
           MOVE    I01-DR (3)          TO  SPA-GMN-I01-DR (3)
      *    保険組み合わせ
           MOVE    I01-HKNCOMBI        TO  SPA-GMN-I01-HKNCOMBI
      *    特定入院
           MOVE    I01-TOKNYUIN        TO  SPA-GMN-I01-TOKNYUIN
      *    特定入院料非該当
           MOVE    I01-TOKHIGAITO      TO  SPA-GMN-I01-TOKHIGAITO
      *    入院料加算
           MOVE    I01-KSN (1)         TO  SPA-GMN-I01-KSN (1)
           MOVE    I01-KSN (2)         TO  SPA-GMN-I01-KSN (2)
           MOVE    I01-KSN (3)         TO  SPA-GMN-I01-KSN (3)
      *    前受け金
      *    MOVE    I01-MAEUKE          TO  SPA-GMN-I01-MAEUKE
      *    定期請求
           MOVE    I01-SEIKYU          TO  SPA-GMN-I01-SEIKYU
      *    検索時患者表示
           MOVE    I01-DISPKBN         TO  SPA-GMN-I01-DISPKBN
      *    ユーザープログラム
           MOVE    I01-USERPG          TO  SPA-GMN-I01-USERPG
      *    入院料算定
           MOVE    I01-NYUCALKBN       TO  SPA-GMN-I01-NYUCALKBN
      *    分娩入院
           MOVE    I01-BUNBENKBN       TO  SPA-GMN-I01-BUNBEN
      *    直接利用制度利用区分
           MOVE    I01-RIYOUKBN        TO  SPA-GMN-I01-RIYOU
      *    選択番号
           MOVE    I01-SELNUM          TO  SPA-GMN-I01-SELNUM
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
           WHEN    "01"
      *
               IF    ( SPA-NAI-I01-MAEIDOYMD   =   SPACE )
      *            入院日チェック処理
                   PERFORM 4101-NYUINYMD-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  4102-KIHON-CHK-EXT
                   END-IF
               END-IF
               MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  WRK-KIHON-KJNYMD
           WHEN    "08"
      *
      *        異動日チェック処理
               PERFORM 4101-IDOYMD-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
               MOVE    SPA-NAI-I01-IDOYMD
                                   TO  WRK-KIHON-KJNYMD
           WHEN    OTHER
               MOVE    SPA-NAI-I01-MAEIDOYMD
                                   TO  WRK-KIHON-KJNYMD
           END-EVALUATE
      *
      *    退院日チェック処理
           PERFORM 4101-TAIINYMD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    病室番号チェック処理
           IF    ( SPA-GMN-I01-BRMNUM  NOT =   SPA-NAI-I01-BRMNUM-MAE )
            OR   ( SPA-GMN-I01-BTUNAME-CNT =   ZERO )
               PERFORM 4101-BRMNUM-CHK-SEC
               MOVE    SPA-GMN-I01-BRMNUM
                                       TO  SPA-NAI-I01-BRMNUM-MAE
               MOVE    SPA-ERRCD       TO  SPA-NAI-I01-BRMNUM-ERR
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           ELSE
               IF    ( SPA-NAI-I01-BRMNUM-ERR
                                   NOT =   SPACE )
                   MOVE    SPA-NAI-I01-BRMNUM-ERR
                                       TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-I01-CUR
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           END-IF
      *
      *    病棟名称チェック処理
           IF    ( SPA-GMN-I01-BTUNAME
                                       NOT =   SPA-NAI-I01-BTUNAME-MAE )
               PERFORM 4101-BTUNAME-CHK-SEC
               MOVE    SPA-GMN-I01-BTUNAME
                                       TO  SPA-NAI-I01-BTUNAME-MAE
               MOVE    SPA-ERRCD       TO  SPA-NAI-I01-BTUNAME-ERR
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           ELSE
               IF    ( SPA-NAI-I01-BTUNAME-ERR
                                   NOT =   SPACE )
                   MOVE    SPA-NAI-I01-BTUNAME-ERR
                                       TO  SPA-ERRCD
                   MOVE    5           TO  SPA-GMN-I01-CUR
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           END-IF
      *
      *    PERFORM 4101-BTUNAME-CHK-SEC
      *    IF    ( SPA-ERRCD   NOT =   SPACE )
      *        GO  TO  4102-KIHON-CHK-EXT
      *    END-IF
      *
      *    室料差額チェック処理
           PERFORM 4101-SAGAKU-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    食事区分チェック処理
           PERFORM 4101-SKJKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-I01-SKJKBN-CD   =   "C" )
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL     ( IDX1    >   3   )
      *
      *            食事区分チェック処理（個別指示）
                   PERFORM 4101-TSKJKBN-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  4102-KIHON-CHK-EXT
                   END-IF
      *
               END-PERFORM
           END-IF
      *
      *    食事時間区分チェック処理
           PERFORM 4101-TIMEKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    選定入院区分チェック処理
           PERFORM 4101-SENTEIKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    入院科チェック処理
           PERFORM 4101-NYUINKA-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    初回チェック処理
           PERFORM 4101-SHOKAI-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    初回番号チェック処理
           PERFORM 4101-SHONUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    介護異動チェック処理
           PERFORM 4101-KAIGO-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    退院事由チェック処理
           PERFORM 4101-TAIINCD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    担当医チェック処理
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   3   )
      *
               PERFORM 4101-DR-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
      *
           END-PERFORM
      *
      *    保険組み合わせチェック処理
           PERFORM 4101-HKNCOMBI-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    特定入院チェック処理
           PERFORM 4101-TOKNYUIN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    特定入院料非該当チェック処理
           PERFORM 4101-TOKHIGAITO-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    入院料算定チェック
           PERFORM 4101-NYUCALKBNCHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    加算チェック処理
           PERFORM VARYING IDX-KSN FROM    1   BY  1
               UNTIL     ( IDX-KSN >   CONST-GMN-KSN-MAX )
                OR       ( SPA-GMN-I01-KSN-SRYCD (IDX-KSN)
                                   =   SPACE )
      *
               PERFORM 4101-KSN-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
      *
           END-PERFORM
      *
      *    前受け金チェック処理
      *    PERFORM 4101-MAEUKE-CHK-SEC
      *    IF    ( SPA-ERRCD   NOT =   SPACE )
      *        GO  TO  4102-KIHON-CHK-EXT
      *    END-IF
      *
      *    定期請求チェック処理
           PERFORM 4101-SEIKYU-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    検索時患者表示チェック処理
           PERFORM 4101-DISPKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    ユーザー起動プログラムチェック処理
           PERFORM 4101-USERPG-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    分娩区分チェック
           PERFORM 4101-BUNBEN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *    直接支払制度利用区分チェック
           PERFORM 4101-RIYOU-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       4100-PTNUM-CHK-SEC              SECTION.
      *
           PERFORM 420-CLEAR-SEC
      *
           MOVE    1               TO  SPA-GMN-I01-CUR
      *
           INITIALIZE                  SPA-NAI-I01-LOCK
                                       SPA-NAI-I01-PTID
                                       SPA-GMN-I01-NAME
                                       SPA-NAI-I01-KANANAME
                                       SPA-GMN-I01-SEX
                                       SPA-NAI-I01-SEX
                                       SPA-GMN-I01-BIRTHDAY
                                       SPA-NAI-I01-BIRTHDAY
                                       SPA-GMN-I01-AGE
                                       SPA-KYOTUKEY
      *
           MOVE    I01-PTNUM       TO  SPA-GMN-I01-PTNUM
      *
           PERFORM 4101-PTNUM-CHK-SEC
           IF  (   SPA-ERRCD   NOT =   SPACE )
            OR (   FLG-END     NOT =   ZERO  )
               CONTINUE
           ELSE
               PERFORM 4101-PTINF-SET-SEC
           END-IF
      *
           .
      *
       4101-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       4101-PTNUM-CHK-SEC              SECTION.
      *
      *    患者番号が入力されていること
           IF    ( SPA-GMN-I01-PTNUM   =   SPACE )
               MOVE    "1001"      TO  SPA-ERRCD
               GO  TO  4101-PTNUM-CHK-EXT
           END-IF
      *
      *    ＊のとき、患者新規登録へ
           IF    ( SPA-GMN-I01-PTNUM(1:1)  =   "*" )
               MOVE    "*"                 TO  SPA-PTNUM
               PERFORM 481-KANJATOROKU-SEC
               GO  TO  4101-PTNUM-CHK-EXT
           END-IF
      *
      *    患者番号変換サブ呼び出し
           INITIALIZE                  ORCSPTNUMAREA
           MOVE    SPA-GMN-I01-PTNUM   TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           MOVE    SPTNUM-PTNUM        TO  SPA-GMN-I01-PTNUM
           EVALUATE    SPTNUM-RC
           WHEN    00
               MOVE    SPTNUM-PTNUM        TO  SPA-GMN-I01-PTNUM
               MOVE    SPTNUM-PTID         TO  SPA-NAI-I01-PTID
           WHEN   10
               MOVE    "0002"              TO  SPA-ERRCD
               GO  TO  4101-PTNUM-CHK-EXT
           WHEN    20
      *        氏名検索へ
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPA-KYOUTU          TO  LNK-COMMON
               MOVE    SPA-I0FREE          TO  LNK-SCRSPA
      *
               MOVE    "I01"               TO  SPA-MOTOPG
               MOVE    "P97"               TO  SPA-SAKIPG
               MOVE    ZERO                TO  SPA-PTID
      *
      *        患者氏名セット
               MOVE    SPA-GMN-I01-PTNUM   TO  SPA-NAME
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
               MOVE    "P97"               TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
               GO  TO  4101-PTNUM-CHK-EXT
           WHEN    OTHER
               MOVE    "0002"              TO  SPA-ERRCD
               GO  TO  4101-PTNUM-CHK-EXT
           END-EVALUATE
      *
      *    患者マスタ検索
           PERFORM 900-PTINF-KEY-SEL-SEC
           IF    ( FLG-PTINF       =   ZERO )
               CONTINUE
           ELSE
               MOVE    "0002"                  TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4101-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴マスタ検索処理（入力領域表示用）
      *****************************************************************
       4101-PTNYUINRRK-SEL1-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNYUINRRK
           INITIALIZE  PTNYUINRRK-REC
           MOVE    ZERO            TO  SPA-NAI-I01-PTRRKNUM
                                       SPA-NAI-I01-PTRRKEDANUM
      *
           MOVE    SPA-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID
                                   TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4101-PTNYUINRRK-SEL1-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       4101-INPUT-AREA-EDIT-SEC        SECTION.
      *
           IF    ( PTNYUINRRK-TAIINYMD =   "99999999" )
               MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
           END-IF
      *
      *    入力領域初期化
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           MOVE    PTNYUINRRK-RRKNUM
                                   TO  SPA-NAI-I01-PTRRKNUM
           MOVE    PTNYUINRRK-RRKEDANUM
                                   TO  SPA-NAI-I01-PTRRKEDANUM
      *
           MOVE    ZERO            TO  SPA-NAI-I01-NYURRK-FLG
      *
      *    同日再入院フラグ
           PERFORM 800-ORCSDNTCHK-SEC
           MOVE    SDNTCHK-DJKBN   TO  SPA-NAI-I01-DJKBN
           MOVE    SDNTCHK-STYMD   TO  SPA-NAI-I01-DJSTYMD
      *
      *    前回異動日
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "01" )
            AND  ( PTNYUINRRK-TAIINYMD     NOT =   "99999999" )
               MOVE    SPACE        TO  SPA-NAI-I01-MAEIDOYMD
               MOVE    ZERO         TO  SPA-NAI-I01-PTRRKNUM
           ELSE
               MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
      *
               IF      ( PTNYUINRRK-JTIKBN = "2" OR "3" OR "4" OR "7" )
                AND    ( PTNYUINRRK-TAIINYMD   = "99999999" )
                   MOVE    WRK-HENYMDG TO  SPA-GMN-I01-MAEIDOYMD
               END-IF
      *
               MOVE    WRK-SYMD        TO  SPA-NAI-I01-MAEIDOYMD
           END-IF
      *
      *    保険組み合わせコンボ設定処理
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-KJNYMD
           PERFORM 4101-CMB-HKNCOMBI-SET-SEC
      *
      *    病室番号
           MOVE   PTNYUINRRK-BRMNUM ( 3 : 6 )
                                   TO  WRK-BRMNUM-IN
           PERFORM 800-ORCSBRMNUM-SEC
           MOVE   WRK-BRMNUM-OT    TO  SPA-GMN-I01-BRMNUM
                                       SPA-NAI-I01-BRMNUM-MAE
                                       SPA-NAI-I01-BRMNUM-MOT
      *
      *    病室情報より病棟コンボ編集
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-KJNYMD
           PERFORM 4101-BRMNUM-SEL-SEC
      *
      *    病棟番号
           MOVE    PTNYUINRRK-BTUNUM
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-BTUNAME-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-BTUNAME
                                       SPA-NAI-I01-BTUNAME-MAE
                                       SPA-NAI-I01-BTUNAME-MOT
      *
      *    入院科
           MOVE    PTNYUINRRK-NYUINKA
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-NYUINKA-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-NYUINKA
           MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I01-RECEKA
           MOVE    PTNYUINRRK-NYUINKA
                                   TO  SPA-NAI-I01-NYUINKA-MOT
      *
      *    保険組合せ番号
           MOVE    SPACE           TO  SPA-GMN-I01-HKNCOMBI
                                       SPA-NAI-I01-FTNRATE
                                       SPA-NAI-I01-RSI-HKNKBN
      *
           MOVE    PTNYUINRRK-HKNCOMBINUM
                                   TO  SPA-GMN-I01-HKNCOMBI-CD
           MOVE    PTNYUINRRK-HKNCOMBINUM
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-HKNCOMBI-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-HKNCOMBI
               MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I01-FTNRATE
               MOVE    WRK-CMB-ITM3    TO  SPA-NAI-I01-RSI-HKNKBN
           END-IF
           MOVE    PTNYUINRRK-HKNCOMBINUM
                                   TO  SPA-NAI-I01-HKNCOMBI-MOT
      *
      *    入院日
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-NYUINYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-I01-NYUINYMD
                                       SPA-NAI-I01-NYUINYMD-MAE
      *
      *    退院日
           IF    ( PTNYUINRRK-TAIINYMD     =   "99999999" )
               MOVE    SPACE           TO  SPA-GMN-I01-TAIINYMD
                                           SPA-NAI-I01-TAIINYMD
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-TAIINYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I01-TAIINYMD
      *
      *        退院事由
               MOVE    PTNYUINRRK-TAIINCD  TO  WRK-CMB-CD
               PERFORM 4103-CMB-TAIINCD-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TAIINCD
           END-IF
      *
      *    特定入院料コンボ設定処理
           MOVE    PTNYUINRRK-TOKUTEI-KBN
                                       TO  SPA-NAI-I01-TOKUTEI-KBN
           MOVE    PTNYUINRRK-TOKUTEI-NYUIN
                                       TO  SPA-NAI-I01-TOKUTEI-NYUIN
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-KJNYMD
           PERFORM 4101-CMB-TOKNYUIN-SET-SEC
      *
      *    特定入院料
           MOVE    SPACE           TO  WRK-CMB-CD
           MOVE    PTNYUINRRK-TOKUTEI-KBN
                                   TO  WRK-CMB-CD (1 : 1)
           MOVE    PTNYUINRRK-TOKUTEI-NYUIN
                                   TO  WRK-CMB-CD (2 : 2)
      *
           PERFORM 4103-CMB-TOKNYUIN-SRH2-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TOKNYUIN
           MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I01-TOKNYUIN-ORG
                                       SPA-NAI-I01-TOKNYUINMOT
           MOVE    WRK-CMB-ITM4    TO  SPA-NAI-I01-TOKNYUINCD
           MOVE    WRK-CMB-ITM5    TO  SPA-NAI-I01-TOKHIGAITOCD
      *
      *    特定入院料非該当
           MOVE    PTNYUINRRK-TOKU-SANTEIYOUKENKBN
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-TOKHIGAITO-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               PERFORM 4103-CMB-TOKHIGAITO-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-TOKHIGAITO
           ELSE
               MOVE    "0"         TO  WRK-CMB-CD
               PERFORM 4103-CMB-TOKHIGAITO-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-TOKHIGAITO
           END-IF
      *
      *    算定入院料
           IF    ( SPA-GMN-I01-TOKNYUIN    =   SPACE )
               MOVE    SPA-NAI-I01-KHNRYO-NM
                                       TO  SPA-GMN-I01-SNTNYUIN
               MOVE    SPA-NAI-I01-KHNRYO-CD
                                       TO  SPA-NAI-I01-SNTNYUINCD
           ELSE
               IF    ( SPA-NAI-I01-TOKHIGAITOCD    NOT =   SPACE )
                AND  ( SPA-GMN-I01-TOKHIGAITO-CD       =   "1" )
                   MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  WRK-KJNYMD
                   MOVE    SPA-NAI-I01-TOKHIGAITOCD
                                       TO  WRK-SRYCD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   MOVE    TNS-NAME    TO  SPA-GMN-I01-SNTNYUIN
                   MOVE    TNS-SRYCD   TO  SPA-NAI-I01-SNTNYUINCD
               ELSE
                   MOVE    SPA-GMN-I01-TOKNYUIN-NM
                                       TO  SPA-GMN-I01-SNTNYUIN
                   MOVE    SPA-NAI-I01-TOKNYUINCD
                                       TO  SPA-NAI-I01-SNTNYUINCD
               END-IF
           END-IF
      *
      *    室料差額
           MOVE    PTNYUINRRK-RM-SAGAKU
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SAGAKU-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-SAGAKU
      *
      *    定期請求区分
           MOVE    PTNYUINRRK-TEIKI-SEIKYUKBN
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SEIKYU-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-SEIKYU
      *
      *    検索表示区分
           MOVE    PTNYUINRRK-KENSAKU-DISPKBN
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-DISPKBN-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-DISPKBN
      *
      *    初回継続区分
           MOVE  PTNYUINRRK-SHOKAIKBN
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SHOKAI-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-SHOKAI
      *
      *    初回番号
           MOVE    PTNYUINRRK-SHONUM
                                   TO  SPA-GMN-I01-SHONUM
      *    初回入院日
           PERFORM 4101-SHONYUINYMD-EDIT-SEC
      *
      *    介護異動区分
           MOVE  PTNYUINRRK-KAIGOKBN
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-KAIGO-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-KAIGO
      *
      *    担当医１
           MOVE    PTNYUINRRK-DRCD1 (2 : 4)
                                   TO  WRK-CMB-CD
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-KJNYMD
           MOVE    1               TO  IDX1
           PERFORM 4103-CMB-DR-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-DR (1)
      *
      *    担当医２
           MOVE    PTNYUINRRK-DRCD2 (2 : 4) 
                                   TO  WRK-CMB-CD
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-KJNYMD
           MOVE    2               TO  IDX1
           PERFORM 4103-CMB-DR-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-DR (2)
      *
      *    担当医３
           MOVE    PTNYUINRRK-DRCD3 (2 : 4)
                                   TO  WRK-CMB-CD
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-KJNYMD
           MOVE    3               TO  IDX1
           PERFORM 4103-CMB-DR-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-DR (3)
      *
      *    食事区分
           MOVE    PTNYUINRRK-SKJKBN
                                   TO  SPA-NAI-I01-MAESKJKBN
           MOVE    PTNYUINRRK-SKJKBN-ASA
                                   TO  SPA-NAI-I01-TMAESKJKBN-ASA
           MOVE    PTNYUINRRK-SKJKBN-HIRU
                                   TO  SPA-NAI-I01-TMAESKJKBN-HIRU
           MOVE    PTNYUINRRK-SKJKBN-YU
                                   TO  SPA-NAI-I01-TMAESKJKBN-YU
           PERFORM 310-CMB-SKJKBN-SET-SEC
           MOVE    PTNYUINRRK-SKJKBN
                                   TO  WRK-CMB-CD
           IF   (( SPA-GMN-I01-SYORIKBN-CD     =   "01" )
              AND( SPA-NAI-I01-MAEIDOYMD   NOT =   SPACE ))
            OR  (  SPA-GMN-I01-SYORIKBN-CD     =   "08" )
               IF    ( PTNYUINRRK-SKJKBN       =   "B" OR "C" )
                   MOVE    "B"     TO  WRK-CMB-CD
               ELSE
                   MOVE    "A"     TO  WRK-CMB-CD
               END-IF
           END-IF
           PERFORM 4103-CMB-SKJKBN-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    "1"         TO  WRK-CMB-CD
               PERFORM 4103-CMB-SKJKBN-CHK-SEC
           END-IF
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-SKJKBN
      *
           IF    ( PTNYUINRRK-SKJKBN        =   "B" OR "C" )
               PERFORM VARYING IDXA    FROM    1   BY  1
                       UNTIL ( IDXA    >   3 )
                   MOVE    PTNYUINRRK-SKJKBN-KOBETSU (IDXA)
                                   TO   WRK-CMB-CD
                   PERFORM 4103-CMB-TSKJKBN-CHK-SEC
                   IF    ( WRK-CMB-ITM  NOT =   SPACE )
                       MOVE    WRK-CMB-ITM
                                   TO   SPA-GMN-I01-TSKJKBN (IDXA)
                   END-IF
               END-PERFORM
           END-IF
      *
      *    食事時間区分
           MOVE    PTNYUINRRK-TIMEKBN-IN
                                   TO  SPA-GMN-I01-TIMEKBN-CD
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "01" )
            AND  ( SPA-NAI-I01-MAEIDOYMD   NOT =   SPACE )
               PERFORM 310-CMB-TIMEKBN-SET-SEC
           END-IF
      *
           MOVE    SPA-GMN-I01-TIMEKBN-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-TIMEKBN-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TIMEKBN
      *
      *    選定入院区分
           MOVE    PTNYUINRRK-SENTEIKBN
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SENTEIKBN-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SENTEIKBN
           ELSE
               MOVE    "1"             TO  WRK-CMB-CD
               PERFORM 4103-CMB-SENTEIKBN-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SENTEIKBN
           END-IF
      *
      *    入院料算定区分
           MOVE    PTNYUINRRK-NYUCALKBN
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-NYUCALKBN-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-NYUCALKBN
           ELSE
               MOVE    "2"             TO  WRK-CMB-CD
               PERFORM 4103-CMB-NYUCALKBN-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-NYUCALKBN
           END-IF
      *
      *    入院料加算
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" )
            AND  ( SPA-NAI-I01-MAEIDOYMD   NOT =   SPACE )
               MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-KJNYMD
               PERFORM 4101-CMB-KSN-SET-SEC
      *
               PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL     ( IDX-KSN >   CONST-GMN-KSN-MAX )
                    OR       ( SPA-GMN-I01-KSN-SRYCD (IDX-KSN)
                                       =   SPACE )
      *
                   MOVE    "1"         TO  WRK-CMB-CD
                   IF    ( SPA-GMN-I01-KSN-SRYCD (IDX-KSN)
                                       =   PTNYUINRRK-KSNSRYCD (1)
                                        OR PTNYUINRRK-KSNSRYCD (2)
                                        OR PTNYUINRRK-KSNSRYCD (3))
                       MOVE    "2"     TO  WRK-CMB-CD
                   END-IF
                   PERFORM 4103-CMB-KSN-CHK-SEC
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-KSN (IDX-KSN)
      *
               END-PERFORM
           END-IF
      *
           PERFORM 41011-BUNBEN-AREA-EDIT-SEC
      *
           .
      *
       4101-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    分娩情報編集処理
      *****************************************************************
       41011-BUNBEN-AREA-EDIT-SEC      SECTION.
      *
           MOVE    SPA-NAI-I01-PTRRKNUM
                                      TO  WRK-BUNBENRRKNUM
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
           IF    ( FLG-BUNBEN     =   ZERO )
               MOVE    1              TO  SPA-NAI-I01-BUNBEN
               MOVE    BUNBEN-BUNBENKBN
                                      TO  WRK-CMB-CD
               PERFORM 4103-CMB-BUNBEN-CHK-SEC
               MOVE    WRK-CMB-ITM    TO  SPA-GMN-I01-BUNBEN
      *
               MOVE    BUNBEN-RIYOUKBN
                                      TO  WRK-CMB-CD
               PERFORM 4103-CMB-RIYOU-CHK-SEC
               MOVE    WRK-CMB-ITM    TO  SPA-GMN-I01-RIYOU
      *
           ELSE
               MOVE    SPACE          TO  SPA-GMN-I01-RIYOU
           END-IF
      *
           .
       41011-BUNBEN-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    初回入院日編集処理
      *****************************************************************
       4101-SHONYUINYMD-EDIT-SEC       SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I01-NYUREKILST-CNT )
               IF    ( SPA-GMN-I01-SHONUM
                                   =   SPA-GMN-I01-TSHONUM (IDX1))
                   IF    ( SPA-GMN-I01-TSHOKAI (IDX1)  =   "1" )
                       STRING "入院日 "
                              SPA-GMN-I01-TNYUINYMD (IDX1)
                       DELIMITED   BY  SIZE
                       INTO   SPA-GMN-I01-LBL-SHONYUINYMD
                       END-STRING
                       MOVE    SPA-GMN-I01-NYUREKILST-CNT
                                   TO  IDX1
                   END-IF
               END-IF
      *
           END-PERFORM
       .
      *
       4101-SHONYUINYMD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理（入院歴を編集）
      *****************************************************************
       4101-INPUT-AREA-EDIT2-SEC       SECTION.
      *
      *    入力領域初期化
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           MOVE    1               TO  SPA-NAI-I01-NYURRK-FLG
      *
           MOVE    PTNYUINRRK-RRKNUM
                                   TO  SPA-NAI-I01-PTRRKNUM
           MOVE    PTNYUINRRK-RRKEDANUM
                                   TO  SPA-NAI-I01-PTRRKEDANUM
      *
      *    同日再入院フラグ
           PERFORM 800-ORCSDNTCHK-SEC
           MOVE    SDNTCHK-DJKBN   TO  SPA-NAI-I01-DJKBN
           MOVE    SDNTCHK-STYMD   TO  SPA-NAI-I01-DJSTYMD
      *
      *    病棟名称
           MOVE    PTNYUINRRK-BTUNAME
                                   TO  SPA-GMN-I01-BTUNAME
      *
      *    入院日
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-NYUINYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-I01-NYUINYMD
                                       SPA-NAI-I01-NYUINYMD-MAE
      *
      *    退院日
           IF    ( PTNYUINRRK-TAIINYMD     =   "99999999" )
               MOVE    SPACE           TO  SPA-GMN-I01-TAIINYMD
                                           SPA-NAI-I01-TAIINYMD
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-TAIINYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I01-TAIINYMD
      *
      *        退院事由
               MOVE    PTNYUINRRK-TAIINCD  TO  WRK-CMB-CD
               PERFORM 4103-CMB-TAIINCD-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TAIINCD
           END-IF
      *
      *    初回継続区分
           MOVE  PTNYUINRRK-SHOKAIKBN
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SHOKAI-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-SHOKAI
      *
      *    初回番号
           MOVE    PTNYUINRRK-SHONUM
                                   TO  SPA-GMN-I01-SHONUM
      *    初回入院日
           PERFORM 4101-SHONYUINYMD-EDIT-SEC
      *
      *    介護異動区分
           MOVE  PTNYUINRRK-KAIGOKBN
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-KAIGO-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-KAIGO
      *
       .
      *
       4101-INPUT-AREA-EDIT2-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴編集処理
      *****************************************************************
       4101-NYUREKI-EDIT-SEC       SECTION.
      *
      *    患者履歴マスタ検索
           PERFORM 4101-PTNYUINRRK-SEL2-SEC
      *
      *    主病名編集処理
           PERFORM 4101-SYUBYOMEI-EDIT-SEC
      *
      *    日数編集処理
           PERFORM 4101-NISSU-EDIT-SEC
      *
           .
       4101-NYUREKI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴編集処理（入院歴表示用）
      *****************************************************************
       4101-PTNYUINRRK-SEL2-SEC        SECTION.
      *
      *
           INITIALIZE  PTNYUINRRK-REC
                       SPA-GMN-I01-NYUREKILST-AREA
                       SPA-NAI-I01-NYUREKILST-AREA
                       WRK-REKI-BYOMEI-EDIT-AREA
      *
           MOVE    1               TO  FLG-NYUREKILST-ROW
      *
           MOVE    SPA-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID
                                   TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
      *
      *    入院履歴検索処理
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1            >   CONST-NYUREKILST-MAX   )
               OR        ( MCP-RC      NOT =   ZERO )
      *
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
               MOVE    IDX1        TO  SPA-GMN-I01-NYUREKILST-CNT
      *
      *
      *        入院歴リスト編集
               IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
                   PERFORM 4101-NYUREKILST-EDIT2-SEC
               ELSE
                   PERFORM 4101-NYUREKILST-EDIT-SEC
               END-IF
      *
      *        入院履歴検索処理
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4101-PTNYUINRRK-SEL2-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       4101-PTINF-SELECT-SEC              SECTION.
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
           IF    ( FLG-PTINF           =   ZERO )
      *
      *        患者情報設定
               PERFORM 4101-PTINF-SET-SEC
           END-IF
      *
      *
           .
       4101-PTINF-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    患者情報設定処理
      *****************************************************************
       4101-PTINF-SET-SEC              SECTION.
      *
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC
      *
      *    患者ＩＤ
           MOVE    PTINF-PTID      TO  SPA-NAI-I01-PTID
      *    患者氏名
           MOVE    PTINF-NAME      TO  SPA-GMN-I01-NAME
      *    患者カナ氏名
           MOVE    PTINF-KANANAME  TO  SPA-NAI-I01-KANANAME
      *    患者性別
           EVALUATE    PTINF-SEX
           WHEN    "1"
               MOVE    "男"        TO  SPA-GMN-I01-SEX
           WHEN    "2"
               MOVE    "女"        TO  SPA-GMN-I01-SEX
           END-EVALUATE
           MOVE    PTINF-SEX       TO  SPA-NAI-I01-SEX
      *    患者生年月日
           MOVE    PTINF-BIRTHDAY  TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-BIRTHDAY
           MOVE    PTINF-BIRTHDAY  TO  SPA-NAI-I01-BIRTHDAY
      *    患者年齢
           MOVE    PTINF-BIRTHDAY  TO  WRK-BIRTHDAY
           PERFORM 4101-AGE-CALC-SEC
      *
           MOVE    WRK-AGE-G       TO  SPA-GMN-I01-AGE
      *
           INITIALIZE                  SS007-AREA
           MOVE    SPA-NAI-I01-PTID TO SPA-PTID
           MOVE    SPA-SYSYMD (1:6) TO SS007-SRYYM
           CALL    "ORCSS007"      USING   SS007-AREA
                                           SPA-AREA
           IF    ( SS007-RC        NOT =   ZERO )
               MOVE    SPA-SYSYMD   TO  WRK-SYMD
               MOVE    SPACE        TO  SPA-GMN-I01-LBLINFO
               STRING  "＊＊　"     DELIMITED  BY  SPACE
                       CONST-IDOMEI DELIMITED  BY  SPACE
                       "処理を行い、今月の入院料を再設定してください"
                                    DELIMITED  BY  SIZE
                       "　＊＊"
                                    DELIMITED  BY  SIZE
               INTO    SPA-GMN-I01-LBLINFO
               END-STRING
           END-IF
      *
      *    療養病棟で特定入院料算定時に、上限日数超えで療養病棟入院料の
      *    算定が行われていない場合の注意メッセージ表示
           IF    SPA-GMN-I01-LBLINFO   =   SPACE
              PERFORM    4101-RYOYO-CHK-SEC
           END-IF
      *
      *    入院履歴編集処理
           PERFORM 4101-NYUREKI-EDIT-SEC
      *
           MOVE    ZERO                TO  FLG-SYORIPTN
      *
      *    入院履歴マスタ検索処理（入力領域表示用）
           PERFORM 4101-PTNYUINRRK-SEL1-SEC
      *    入院歴なし（初入院）
           IF  (   FLG-PTNYUINRRK  NOT =   ZERO )
               MOVE    1               TO  FLG-SYORIPTN
           ELSE
      *
               EVALUATE    TRUE
      *        歴作成データあり
               WHEN    PTNYUINRRK-JTIKBN   =   "5"
               WHEN    PTNYUINRRK-JTIKBN   =   "6"
                   MOVE    2           TO  FLG-SYORIPTN
      *        入院中
               WHEN    PTNYUINRRK-TAIINYMD =   "99999999"
                   MOVE    3           TO  FLG-SYORIPTN
      *        退院済み
               WHEN    OTHER
                   MOVE    4           TO  FLG-SYORIPTN
               END-EVALUATE
           END-IF
      *
           IF    ( SPA-ERRCD           =   "9999" )
               MOVE        1           TO  SPA-NAI-I01-LOCK
               IF    ( FLG-SYORIPTN    =   1 )
                   MOVE    5           TO  FLG-SYORIPTN
               ELSE
                   MOVE    6           TO  FLG-SYORIPTN
               END-IF
           ELSE
               MOVE        ZERO        TO  SPA-NAI-I01-LOCK
           END-IF
      *
           EVALUATE    FLG-SYORIPTN
           WHEN    1
      *
               MOVE    3           TO  SPA-GMN-I01-CUR
      *
      *        デフォルトの処理区分に入院登録を設定
               MOVE    "01"        TO  SPA-GMN-I01-SYORIKBN-CD
      *
               MOVE    SPA-SYSYMD  TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
               PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *        保険組み合わせコンボ設定処理
               MOVE    SPA-SYSYMD      TO  WRK-KJNYMD
               PERFORM 4101-CMB-HKNCOMBI-SET-SEC
      *        受付マスタより入院科等を確定する
               MOVE    SPA-SYSYMD  TO  WRK-KJNYMD
               PERFORM 4101-UKETUKE-SEL-SEC
      *        受診履歴検索処理
               MOVE    SPA-SYSYMD  TO  WRK-KJNYMD
               PERFORM 4101-JYURRK-SEL-SEC
               PERFORM 310-CMB-SYORIKBN-SET2-SEC
               MOVE    SPA-GMN-I01-SYORIKBN-CD
                                   TO  WRK-CMB-CD
               PERFORM 4103-CMB-SYORIKBN-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SYORIKBN
           WHEN    2
               MOVE    2           TO  SPA-GMN-I01-CUR
      *
      *        デフォルトの処理区分に照会を設定
               MOVE    "04"        TO  SPA-GMN-I01-SYORIKBN-CD
      *
               PERFORM 4101-INPUT-AREA-EDIT2-SEC
               PERFORM 310-CMB-SYORIKBN-SET5-SEC
               MOVE    SPA-GMN-I01-SYORIKBN-CD
                                   TO  WRK-CMB-CD
               PERFORM 4103-CMB-SYORIKBN-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SYORIKBN
      *
      *
           WHEN    3
               MOVE    2           TO  SPA-GMN-I01-CUR
      *
      *        デフォルトの処理区分に変更を設定
               MOVE    "03"        TO  SPA-GMN-I01-SYORIKBN-CD
      *
               PERFORM 4101-INPUT-AREA-EDIT-SEC
               IF    ( PTNYUINRRK-RRKEDANUM    >   1   )
                   PERFORM 310-CMB-SYORIKBN-SET6-SEC
               ELSE
                   PERFORM 310-CMB-SYORIKBN-SET3-SEC
               END-IF
               MOVE    SPA-GMN-I01-SYORIKBN-CD
                                   TO  WRK-CMB-CD
               PERFORM 4103-CMB-SYORIKBN-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SYORIKBN
           WHEN    4
               MOVE    2               TO  SPA-GMN-I01-CUR
      *
      *        デフォルトの処理区分に照会を設定
               MOVE    "04"        TO  SPA-GMN-I01-SYORIKBN-CD
      *
               PERFORM 4101-INPUT-AREA-EDIT-SEC
      *
               PERFORM 310-CMB-SYORIKBN-SET4-SEC
               MOVE    SPA-GMN-I01-SYORIKBN-CD
                                   TO  WRK-CMB-CD
               PERFORM 4103-CMB-SYORIKBN-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SYORIKBN
      *
           WHEN    5
               MOVE    1               TO  SPA-GMN-I01-CUR
               INITIALIZE                  SPA-GMN-I01-CMB-SYORIKBN
                                           SPA-GMN-I01-SYORIKBN
      *
           WHEN    6
      *
      *        デフォルトの処理区分に照会を設定
               MOVE    "04"        TO  SPA-GMN-I01-SYORIKBN-CD
               MOVE    1               TO  SPA-GMN-I01-CUR
      *
               PERFORM 4101-INPUT-AREA-EDIT-SEC
      *
               PERFORM 310-CMB-SYORIKBN-SET7-SEC
               MOVE    SPA-GMN-I01-SYORIKBN-CD
                                   TO  WRK-CMB-CD
               PERFORM 4103-CMB-SYORIKBN-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SYORIKBN
           END-EVALUATE
      *
           IF    ( SPA-GMN-I01-NYUREKILST-CNT  >   ZERO )
               MOVE    1           TO  SPA-GMN-I01-NYUREKILST-SEL
                                       SPA-GMN-I01-SELNUM
           END-IF
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" )
            AND  ( SPA-NAI-I01-MAEIDOYMD   =   SPACE )
               MOVE    ZERO        TO  SPA-NAI-I01-PTRRKNUM
                                       SPA-NAI-I01-PTRRKEDANUM
           END-IF
      D    DISPLAY "SPA-NAI-I01-PTRRKNUM=" SPA-NAI-I01-PTRRKNUM
      *
           MOVE    SPA-GMN-I01-PTNUM   TO  SPA-LAST-PTNUM
           MOVE    SPA-NAI-I01-PTID    TO  SPA-LAST-PTID
      *
           .
      *
       4101-PTINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    療養病棟の特定入院料算定上限日数超えチェック
      *****************************************************************
       4101-RYOYO-CHK-SEC            SECTION.
      *
      *    療養病棟で特定入院料算定時に、上限日数超えで療養病棟入院料の
      *    算定が行われていない場合の注意メッセージ表示
      *    １．最終入院履歴から入院病棟が療養病棟であるか判定
      *    ２．入院会計テーブルの作成済最終月から剤識別区分１（入院料）
      *        のデータを取得（剤識別区分１は複数あり）
      *    ３．該当月の最終日カレンダーに１が記録されていない場合、
      *        エラーメッセージのセット
      *    注意：入退院登録画面で「１入院料を算定しない」に設定されている
      *          場合は、エラーメッセージを表示しない
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
      *     入院中患者の療養病棟（病床）の判定
      *    （最終入院履歴が歴作成した自院歴、他院歴の場合は処理しない）
            IF   PTNYUINRRK-JTIKBN   =    "5"   OR  "6"
                 CONTINUE
            ELSE
      *       （入退院登録画面で入院料を算定する設定時のみ）
              IF   (PTNYUINRRK-TAIINYMD    =   "99999999")   AND
                   (PTNYUINRRK-NYUCALKBN   =   "2")
                INITIALIZE                      SYS-5001-REC
                MOVE    "5001"              TO  SYS-5001-KANRICD
                MOVE    PTNYUINRRK-BTUNUM   TO  SYS-5001-KBNCD
                MOVE    PTNYUINRRK-TENNYUYMD
                                            TO  SYS-5001-STYUKYMD (1:6)
                MOVE    "01"                TO  SYS-5001-STYUKYMD (7:2)
                MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
                MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
                MOVE    SYS-5001-REC        TO  MCPDATA-REC
                MOVE    "tbl_syskanri"      TO  MCP-TABLE
                MOVE    "key10"             TO  MCP-PATHNAME
                PERFORM 910-DBSELECT-SEC
                IF      MCP-RC              =   ZERO
      *            最終入院履歴が療養病棟（病床）の時、特定入院料で
      *            あるか判定を行い、そうであれば入院会計の参照して
      *            会計最終日のカレンダーに１が立っているかをチェック
                   IF    SYS-5001-BTU-SBT    =   "04"  OR  "09"
                     INITIALIZE              ORCSTOKNYUIN-AREA
                     MOVE    PTNYUINRRK-TOKUTEI-KBN
                                           TO  LNK-TOKNYUIN-KBN
                     MOVE    PTNYUINRRK-TOKUTEI-NYUIN
                                           TO  LNK-TOKNYUIN-NYUIN
                     MOVE    PTNYUINRRK-BTUNUM
                                           TO  LNK-TOKNYUIN-BTUNUM
                     MOVE    PTNYUINRRK-TOKU-SANTEIYOUKENKBN
                                       TO  LNK-TOKNYUIN-SANTEIYOUKENKBN
                     MOVE    PTNYUINRRK-TENNYUYMD
                                           TO  LNK-TOKNYUIN-SANTEIYM
                     CALL    "ORCSTOKNYUIN"  USING ORCSTOKNYUIN-AREA
                                                   SPA-AREA
                     IF      LNK-TOKNYUIN-RC    =   ZERO
                       IF    LNK-TOKNYUIN-SRYCD   NOT =  SPACE
      *                    点数マスタを参照し、特定入院料か判定
                           INITIALIZE                TNS-TENSU-REC
                           MOVE   SPA-HOSPNUM    TO  TNS-HOSPNUM
                           MOVE   LNK-TOKNYUIN-SRYCD
                                                 TO  TNS-SRYCD
                           MOVE   PTNYUINRRK-TENNYUYMD
                                                 TO  TNS-YUKOSTYMD
                                                     TNS-YUKOEDYMD
                           MOVE   TNS-TENSU-REC  TO  MCPDATA-REC
                           MOVE   "tbl_tensu"    TO  MCP-TABLE
                           MOVE   "key"          TO  MCP-PATHNAME
                           PERFORM 910-DBSELECT-SEC
                           IF      MCP-RC        =   ZERO
                             MOVE    MCPDATA-REC TO  TNS-TENSU-REC
                             IF   TNS-NYUTENTTLKBN   =   920
      *                        短期滞在はチェック対象外
                               IF    (TNS-CDKBN-KBN     =   "A")   AND
                                     (TNS-CDKBN-KBNNUM  =   400)   AND
                                     (TNS-CDKBN-KOUBAN  =   2  OR 3)
                                 CONTINUE
                               ELSE
      *                          入院会計を検索
                                 INITIALIZE               NYUINACCT-REC
                                 MOVE  SPA-HOSPNUM    TO  NACCT-HOSPNUM
                                 MOVE  "1"            TO
                                                         NACCT-NYUGAIKBN
                                 MOVE  SPA-NAI-I01-PTID
                                                      TO NACCT-PTID
                                 MOVE  NYUINACCT-REC  TO  MCPDATA-REC
                                 MOVE  "tbl_nyuinacct"
                                                      TO  MCP-TABLE
                                 MOVE  "key55"        TO  MCP-PATHNAME
                                 PERFORM 910-NYUINACCT-SELECT-SEC
                                 IF    FLG-NYUINACCT  =   ZERO
                                   MOVE  "tbl_nyuinacct"
                                                      TO  MCP-TABLE
                                   MOVE  "key55"      TO  MCP-PATHNAME
                                   PERFORM 910-NYUINACCT-READ-SEC
                                   MOVE   NACCT-SRYYM TO  WRK-CHK2-SRYYM
      *                            月末日を取得
                                   INITIALIZE             STS-AREA-DAY
                                   INITIALIZE             LNK-DAY7-AREA
                                   MOVE  "71"         TO  LNK-DAY7-IRAI
                                   MOVE  WRK-CHK2-SRYYM
                                                      TO  LNK-DAY7-YM
                                   CALL "ORCSDAY"    USING
                                                     STS-AREA-DAY
                                                     LNK-DAY7-AREA
                                   MOVE  LNK-DAY7-KOYOMI
                                                      TO  IDX-MATSUBI
                                   MOVE  ZERO         TO  WRK-CHK2-FLG
                                   PERFORM UNTIL
                                          (FLG-NYUINACCT =  1)  OR
                                          (NACCT-SRYYM   NOT = 
                                                        WRK-CHK2-SRYYM)
                                      IF   NACCT-DAY(IDX-MATSUBI)
                                                      NOT =   ZERO
                                           MOVE  1    TO   WRK-CHK2-FLG
                                      END-IF
                                      MOVE    "tbl_nyuinacct"
                                                      TO  MCP-TABLE
                                      MOVE    "key55" TO  MCP-PATHNAME
                                      PERFORM 910-NYUINACCT-READ-SEC
                                   END-PERFORM
      *
                                   IF    WRK-CHK2-FLG   =   ZERO
                                     MOVE  SPACE TO  SPA-GMN-I01-LBLINFO
               STRING  "特定入院料算定後に入院料未算定日が"
                                    DELIMITED  BY  SIZE
                       "あります。異動処理を行い"
                                    DELIMITED  BY  SIZE
                       "入院料を算定してください。"
                                    DELIMITED  BY  SIZE
               INTO    SPA-GMN-I01-LBLINFO
               END-STRING
                                   END-IF
                                 END-IF
                                 MOVE    "tbl_nyuinacct"
                                                      TO  MCP-TABLE
                                 MOVE    "key55"      TO  MCP-PATHNAME
                                 PERFORM 910-DBCLOSECURSOR-SEC
                               END-IF
                             END-IF
                           END-IF
                           MOVE    "tbl_tensu"      TO  MCP-TABLE
                           MOVE    "key"            TO  MCP-PATHNAME
                           PERFORM 910-DBCLOSECURSOR-SEC
                       END-IF
                     END-IF
                   END-IF
                END-IF
                MOVE  MCPDATA-REC   TO  SYS-5001-REC
                MOVE    "tbl_syskanri"      TO  MCP-TABLE
                MOVE    "key10"             TO  MCP-PATHNAME
                PERFORM 910-DBCLOSECURSOR-SEC
              END-IF
            END-IF
           END-IF
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       4101-RYOYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受付マスタ検索処理
      *****************************************************************
       4101-UKETUKE-SEL-SEC            SECTION.
      
      *
      *    受付マスタ検索
           MOVE    SPACE               TO  UKETUKE-REC
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPNUM          TO  UKE-HOSPNUM
           MOVE    WRK-KJNYMD          TO  UKE-UKEYMD
           MOVE    SPA-NAI-I01-PTID    TO  UKE-PTID
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           MOVE    ZERO                TO  FLG-UKETUKEGAITO   
      *
           PERFORM UNTIL ( MCP-RC      NOT =   ZERO )
               OR        ( FLG-UKETUKEGAITO
                                       NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  UKETUKE-REC
      *
      *        会計のまだの最初の受付を対象とする
               IF    ( UKE-KAIKEITIME  =   ZERO )
                   IF    ( SPA-GMN-I01-NYUINKA =   SPACE   )
                       MOVE    1               TO  FLG-UKETUKEGAITO
                       MOVE    UKE-SRYKA       TO  WRK-CMB-CD
                       PERFORM 4103-CMB-NYUINKA-CHK-SEC
                       MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-NYUINKA
                       MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I01-RECEKA
                   END-IF
      *
                   IF    ( SPA-GMN-I01-DR (1)  =   SPACE   )
                       MOVE    UKE-DRCD (2 : 4)
                                               TO  WRK-CMB-CD
                       MOVE    1               TO  IDX1
                       PERFORM 4103-CMB-DR-CHK-SEC
                       MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-DR (1)
                   END-IF
               END-IF
      *
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
           END-PERFORM
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       4101-UKETUKE-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院歴リスト編集処理
      *****************************************************************
       4101-NYUREKILST-EDIT-SEC        SECTION.
      *
      *    自院他院フラグ
           MOVE    ZERO            TO  SPA-NAI-I01-TZITA-FLG (IDX1)
      *
      *
      *    システム管理マスタ検索処理
           MOVE    "5000"          TO  WRK-KANRICD
           MOVE    "*"             TO  WRK-KBNCD
           MOVE    SPA-SYSYMD      TO  WRK-YUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5000-REC
      *
           IF    ( SYS-5000-REKI-DISPKBN   =   "2" )
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  PTNYUINRRK-REC

               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *    同日再入院フラグ
           PERFORM 800-ORCSDNTCHK-SEC
           MOVE    SDNTCHK-DJKBN   TO  SPA-NAI-I01-TDJKBN  (IDX1)
           MOVE    SDNTCHK-STYMD   TO  SPA-NAI-I01-TDJSTYMD(IDX1)
      *
      *    入院歴
           MOVE    PTNYUINRRK-RRKNUM
                                   TO  SPA-NAI-I01-TRRKNUM (IDX1)
      *    選択番号
           MOVE    IDX1            TO  SPA-GMN-I01-TSELNUM (IDX1)
      *
      *    初回区分
           MOVE    PTNYUINRRK-SHOKAIKBN
                                   TO  SPA-GMN-I01-TSHOKAI (IDX1)
      *
      *    介護異動区分
           MOVE    PTNYUINRRK-KAIGOKBN
                                   TO  SPA-GMN-I01-TKAIGO (IDX1)
      *
      *    初回番号
           MOVE    PTNYUINRRK-SHONUM
                                   TO  SPA-GMN-I01-TSHONUM (IDX1)
      *
      *    病棟
           MOVE    "5001"              TO  WRK-KANRICD
           MOVE    PTNYUINRRK-BTUNUM   TO  WRK-KBNCD
           MOVE    PTNYUINRRK-TENNYUYMD TO WRK-YUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5001-REC
           MOVE    SYS-5001-BTU-NAME
                                   TO  SPA-GMN-I01-TBTUNAME (IDX1)
      *
      *    病室番号
           IF    ( PTNYUINRRK-BRMNUM (3 : 6)
                                   IS  NUMERIC )
               MOVE    PTNYUINRRK-BRMNUM (3 : 6)
                                   TO   WRK-999999
               MOVE    WRK-999999
                                   TO   WRK-ZZZZZ9
               MOVE    WRK-ZZZZZ9
                                   TO  SPA-GMN-I01-TBRMNUM (IDX1)
           ELSE
               MOVE    PTNYUINRRK-BRMNUM ( 3 : 6 )
                                   TO  SPA-GMN-I01-TBRMNUM (IDX1)
           END-IF
      *
      *    入院日
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-TNYUINYMD (IDX1)
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  SPA-NAI-I01-TNYUINYMD (IDX1)
      *
      *    退院日
           IF    ( PTNYUINRRK-TAIINYMD   =   "99999999" )
               MOVE    SPACE   TO  SPA-GMN-I01-TTAIINYMD (IDX1)
                                   SPA-NAI-I01-TTAIINYMD (IDX1)
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-I01-TTAIINYMD (IDX1)
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  SPA-NAI-I01-TTAIINYMD (IDX1)
           END-IF
      *
      *    診療科
           MOVE    "1005"              TO  WRK-KANRICD
           MOVE    PTNYUINRRK-NYUINKA  TO  WRK-KBNCD
           MOVE    PTNYUINRRK-TENNYUYMD TO WRK-YUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1005-REC
           MOVE    SYS-1005-SRYKANAME
                                   TO  SPA-GMN-I01-TNYUINKA (IDX1)
      *
      *------------
           MOVE    ZERO            TO  WRK-REKI-BYO-ZITA-FLG (IDX1)
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-REKI-BYO-NYUINYMD  (IDX1)
      *
           IF    ( PTNYUINRRK-TAIINYMD   =   "99999999" )
               MOVE    "99999999"
                                   TO  WRK-REKI-BYO-TAIINYMD (IDX1)
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  WRK-REKI-BYO-TAIINYMD (IDX1)
           END-IF
      *
           MOVE    PTNYUINRRK-NYUINKA
                                   TO  WRK-REKI-BYO-NYUINKA (IDX1)
      *
           MOVE    PTNYUINRRK-RRKNUM
                                   TO  WRK-BUNBENRRKNUM
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
           IF    ( FLG-BUNBEN      =   ZERO )
               IF    ( BUNBEN-BUNBENKBN    =   "1" OR "2" )
                AND((( PTNYUINRRK-TAIINYMD NOT =   "99999999" )
                AND  ( PTNYUINRRK-TAIINYMD    >=   CONST-H211001 ))
                 OR ( PTNYUINRRK-NYUINYMD     >=   CONST-H211001 ))
                   MOVE    1           TO  SPA-NAI-I01-TBUNBEN (IDX1)
               END-IF
           END-IF
      *
           .
      *
       4101-NYUREKILST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院歴リスト編集処理(入院歴を編集)
      *****************************************************************
       4101-NYUREKILST-EDIT2-SEC       SECTION.
      *
      *    自院他院フラグ
           IF    ( PTNYUINRRK-JTIKBN   =   "5" )
               MOVE    1           TO  SPA-NAI-I01-TZITA-FLG (IDX1)
           ELSE
               MOVE    2           TO  SPA-NAI-I01-TZITA-FLG (IDX1)
           END-IF
      *
      *    同日再入院フラグ
           PERFORM 800-ORCSDNTCHK-SEC
           MOVE    SDNTCHK-DJKBN   TO  SPA-NAI-I01-TDJKBN  (IDX1)
           MOVE    SDNTCHK-STYMD   TO  SPA-NAI-I01-TDJSTYMD(IDX1)
      *
      *    入院歴
           MOVE    PTNYUINRRK-RRKNUM
                                   TO  SPA-NAI-I01-TRRKNUM (IDX1)
      *    選択番号
           MOVE    IDX1            TO  SPA-GMN-I01-TSELNUM (IDX1)
      *
      *    初回区分
           MOVE    PTNYUINRRK-SHOKAIKBN
                                   TO  SPA-GMN-I01-TSHOKAI (IDX1)
      *
      *    介護異動区分
           MOVE    PTNYUINRRK-KAIGOKBN
                                   TO  SPA-GMN-I01-TKAIGO (IDX1)
      *
      *    初回番号
           MOVE    PTNYUINRRK-SHONUM
                                   TO  SPA-GMN-I01-TSHONUM (IDX1)
      *
      *    病棟
           MOVE    PTNYUINRRK-BTUNAME
                                   TO  SPA-GMN-I01-TBTUNAME (IDX1)
      *
      *    病室番号（自院歴／他院歴）を表示
           EVALUATE    PTNYUINRRK-JTIKBN
           WHEN    "5"
               MOVE    "他院歴"    TO  SPA-GMN-I01-TBRMNUM (IDX1)
           WHEN    "6"
               MOVE    "自院歴"    TO  SPA-GMN-I01-TBRMNUM (IDX1)
           END-EVALUATE
      *
      *    入院日
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-TNYUINYMD (IDX1)
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  SPA-NAI-I01-TNYUINYMD (IDX1)
      *
      *    退院日（最新の情報を取得）
           IF    ( PTNYUINRRK-TAIINYMD     =   "99999999" )
               MOVE    SPACE       TO  SPA-GMN-I01-TTAIINYMD (IDX1)
                                       SPA-NAI-I01-TTAIINYMD (IDX1)
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-I01-TTAIINYMD (IDX1)
      *
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  SPA-NAI-I01-TTAIINYMD (IDX1)
           END-IF
      *
           IF    ( PTNYUINRRK-JTIKBN       =   "6" )
            AND  ( PTNYUINRRK-NYUINCHUKBN  =   "1" )
               MOVE    "*"         TO  SPA-GMN-I01-TTAIINYMD (IDX1)
                                                             (10:)
           END-IF
      *
      *    通算対象日数
           MOVE    PTNYUINRRK-TAISYONISSU
                                   TO  SPA-NAI-I01-TNYURRKTAISYO (IDX1)
      *
      *    病名
           INITIALIZE  PTTAINFUKA-REC
           MOVE    SPA-HOSPNUM      TO  PTTAINFUKA-HOSPNUM
           MOVE    PTNYUINRRK-PTID
                                   TO  PTTAINFUKA-PTID
           MOVE    PTNYUINRRK-RRKNUM
                                   TO  PTTAINFUKA-RRKNUM
           MOVE    PTTAINFUKA-REC  TO  MCPDATA-REC
           MOVE    "tbl_pttainfuka" TO MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTTAINFUKA-REC
               MOVE    PTTAINFUKA-BYOMEI (1)
                                   TO  WRK-REKI-BYO-BYOMEI (IDX1)
           END-IF
      *
           MOVE    "tbl_pttainfuka" TO MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    1               TO  WRK-REKI-BYO-ZITA-FLG (IDX1)
           MOVE    SPACE           TO  WRK-REKI-BYO-NYUINYMD  (IDX1)
                                       WRK-REKI-BYO-TAIINYMD (IDX1)
                                       WRK-REKI-BYO-TAIINYMD  (IDX1)
                                       WRK-REKI-BYO-NYUINKA (IDX1)
      *
           .
      *
       4101-NYUREKILST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    主病名編集処理
      *****************************************************************
       4101-SYUBYOMEI-EDIT-SEC        SECTION.
      *
      *    患者病名マスタ検索
           INITIALIZE  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM          TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           PERFORM UNTIL ( MCP-RC      NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
      *
      *        入院病名のみを対象とする
               IF    ( PTBYO-NYUGAIKBN =   "1" )
      *            対象の主病名をワークエリアに退避する
                   PERFORM 4101-SYUBYOMEI-TAIHI-SEC
               END-IF
      *
               MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    入院歴リストの主病名の編集を行う
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-NYUREKILST-CNT )
      *
               IF    ( WRK-REKI-BYO-BYOMEI (IDX1)  NOT =   SPACE )
                   MOVE    WRK-REKI-BYO-BYOMEI (IDX1)
                                       TO  SPA-GMN-I01-TBYOMEI (IDX1)
                   EVALUATE    WRK-REKI-BYO-TENKIKBN (IDX1)
                   WHEN "1"
                       MOVE    "治ゆ"  TO  SPA-GMN-I01-TTENKI (IDX1)
                   WHEN "2"
                       MOVE    "死亡"  TO  SPA-GMN-I01-TTENKI (IDX1)
                   WHEN "3"
                       MOVE    "中止"  TO  SPA-GMN-I01-TTENKI (IDX1)
                   WHEN "4"
                       MOVE    "移行"  TO  SPA-GMN-I01-TTENKI (IDX1)
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4101-SYUBYOMEI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    主病名退避処理
      *****************************************************************
       4101-SYUBYOMEI-TAIHI-SEC        SECTION.
      *
           PERFORM VARYING IDX-RRKBYO  FROM    1   BY  1
                   UNTIL ( IDX-RRKBYO  >   SPA-GMN-I01-NYUREKILST-CNT )
      *
               IF  ( WRK-REKI-BYO-ZITA-FLG (IDX-RRKBYO)
                                   =  ZERO             )
                 AND ( WRK-REKI-BYO-NYUINYMD (IDX-RRKBYO)
                                  <=   PTBYO-SRYYMD    )
                 AND ( WRK-REKI-BYO-TAIINYMD (IDX-RRKBYO)
                                  >=   PTBYO-SRYYMD    )
                 AND ( WRK-REKI-BYO-NYUINKA  (IDX-RRKBYO)
                                   =   PTBYO-SRYKA     )
                 AND ( PTBYO-SYUBYOFLG =   "1"             )
      *
                   MOVE    ZERO        TO  FLG-TAISYO
      *
                   EVALUATE    TRUE
                   WHEN    ( WRK-REKI-BYO-SRYYMD (IDX-RRKBYO)
                                   = SPACE )
                       MOVE    1       TO  FLG-TAISYO
                   WHEN    ( WRK-REKI-BYO-SRYYMD (IDX-RRKBYO)
                                   >   PTBYO-SRYYMD    )
                       MOVE    1       TO  FLG-TAISYO
                   WHEN    ( WRK-REKI-BYO-SRYYMD (IDX-RRKBYO)
                                   =   PTBYO-SRYYMD    )
      *                同一日の場合は連番の小さい方を取得
                       IF  ( WRK-REKI-BYO-RENNUM (IDX-RRKBYO)
                                       >   PTBYO-RENNUM )
                           MOVE    1   TO  FLG-TAISYO
                       END-IF
      *
                   END-EVALUATE
      *
      *
                   IF    ( FLG-TAISYO  =   "1" )
                       MOVE    PTBYO-RENNUM
                           TO  WRK-REKI-BYO-RENNUM (IDX-RRKBYO)
                       MOVE    PTBYO-SRYYMD
                           TO  WRK-REKI-BYO-SRYYMD (IDX-RRKBYO)
      *
                       INITIALIZE              ORCSMKBYOMEIAREA
                       MOVE    PTBYO-UTAGAIFLG
                                           TO  SMKBYO-UTAGAIFLG
                       MOVE    PTBYO-HOSOKU-COMMENT
                                           TO  SMKBYO-HOSOKU-COMMENT
                       MOVE    PTBYO-BYOMEI
                                           TO  SMKBYO-BYOMEI
      *
                       CALL    "ORCSMKBYOMEI"   USING
                                                ORCSMKBYOMEIAREA
                       MOVE    SMKBYO-BYOMEI
                           TO  WRK-REKI-BYO-BYOMEI (IDX-RRKBYO)
                       MOVE    PTBYO-TENKIKBN
                           TO  WRK-REKI-BYO-TENKIKBN (IDX-RRKBYO)
                   END-IF
      *
                   MOVE    SPA-GMN-I01-NYUREKILST-CNT
                                       TO  IDX-RRKBYO
      *
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4101-SYUBYOMEI-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    日数編集処理処理
      *****************************************************************
       4101-NISSU-EDIT-SEC             SECTION.
      *
           PERFORM VARYING IDX1
                   FROM    SPA-GMN-I01-NYUREKILST-CNT
                   BY      -1
                   UNTIL ( IDX1    <   1 )
      *
               IF    ( SPA-GMN-I01-TSHOKAI (IDX1)  =   "1" )
                   MOVE    ZERO    TO  WRK-TSUSAN
                   MOVE    SPA-GMN-I01-TSHONUM (IDX1)
                                   TO  WRK-CHK-SHONUM
                   PERFORM VARYING IDX2    FROM    IDX1    BY  -1
                           UNTIL ( IDX2    <   1 )
      *
                       PERFORM 4101-NISSU-EDIT2-SEC
      *
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           IF    ( SPA-GMN-I01-NYUREKILST-CNT  <= ZERO )
               GO  TO  4101-NISSU-EDIT-EXT
           END-IF
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID
                                       TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-I01-TNYUINYMD (SPA-GMN-I01-NYUREKILST-CNT)
                                       TO  PTNYUINRRK-NYUINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key31"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           PERFORM UNTIL ( MCP-RC      NOT =   ZERO )
      *
               PERFORM 800-ORCSDNTCHK-SEC
      *
               MOVE    ZERO            TO  WRK-TSUSAN
               MOVE    PTNYUINRRK-SHONUM
                                       TO  WRK-CHK-SHONUM
      *
               PERFORM UNTIL ( PTNYUINRRK-SHONUM
                                           NOT =   WRK-CHK-SHONUM )
                        OR   ( MCP-RC      NOT =   ZERO )
      *
                   IF    ( PTNYUINRRK-JTIKBN   =   "5" )
                           COMPUTE WRK-TSUSAN
                               =   WRK-TSUSAN
                               +   PTNYUINRRK-TAISYONISSU
                   ELSE
                       IF    ( SDNTCHK-STYMD   NOT =   ZERO )
                           MOVE    SDNTCHK-STYMD
                                       TO  WRK-STYMD
                           MOVE    PTNYUINRRK-TAIINYMD
                                       TO  WRK-EDYMD
                           PERFORM 5002-KIKAN-CALC-SEC
                           IF    ( FLG-ORCSDAY =   ZERO )
                               COMPUTE WRK-TSUSAN
                                   =   WRK-TSUSAN
                                   +   WRK-NISSU2
                           END-IF
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                   MOVE    "key31"             TO  MCP-PATHNAME
                   PERFORM 910-DBFETCH-SEC
                   IF    ( MCP-RC          =   ZERO )
                       MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
                   ELSE
                       INITIALIZE                  PTNYUINRRK-REC
                   END-IF
      *
               END-PERFORM
      *
               PERFORM VARYING IDX2    
                       FROM    SPA-GMN-I01-NYUREKILST-CNT
                       BY      -1
                       UNTIL ( IDX2    <   1 )
      *
                   PERFORM 4101-NISSU-EDIT2-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key31"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4101-NISSU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    日数編集処理処理（対象チェック）
      *****************************************************************
       4101-NISSU-EDIT2-SEC            SECTION.
      *
           IF    ( WRK-CHK-SHONUM  NOT =   SPA-GMN-I01-TSHONUM (IDX2) )
               GO  TO  4101-NISSU-EDIT2-EXT
           END-IF
      *
           IF    ( IDX2    =   1 )
             AND ( SPA-NAI-I01-TTAIINYMD (IDX2)
                               =   SPACE )
               GO  TO  4101-NISSU-EDIT2-EXT
           END-IF
      *
           MOVE    SPA-NAI-I01-TNYUINYMD (IDX2)
                           TO  WRK-STYMD
           MOVE    SPA-NAI-I01-TTAIINYMD (IDX2)
                           TO  WRK-EDYMD
           PERFORM 5002-KIKAN-CALC-SEC
           IF    ( FLG-ORCSDAY =   ZERO )
               IF    ( SPA-NAI-I01-TZITA-FLG (IDX2)
                               =   ZERO )
                   MOVE    WRK-NISSU2
                               TO  WRK-NISSU
      *
                   IF    ( SPA-NAI-I01-TDJKBN (IDX2)   =   SPACE OR "2")
                       COMPUTE WRK-TSUSAN
                           =   WRK-TSUSAN  +   WRK-NISSU2
                   ELSE
                       COMPUTE WRK-TSUSAN
                           =   WRK-TSUSAN  +   WRK-NISSU2  -   1
                   END-IF
      *
               ELSE
                   MOVE    SPA-NAI-I01-TNYURRKTAISYO (IDX2)
                               TO  WRK-NISSU
                   COMPUTE WRK-TSUSAN
                       =   WRK-TSUSAN
                       +   SPA-NAI-I01-TNYURRKTAISYO (IDX2)
      *
               END-IF
               MOVE    WRK-NISSU   TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9   TO  SPA-GMN-I01-TNISSU (IDX2)
               MOVE    WRK-TSUSAN  TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9   TO  SPA-GMN-I01-TTUSAN (IDX2)
           END-IF
      *
           .
       4101-NISSU-EDIT2-EXT.
           EXIT.
      *****************************************************************
      *    年齢算出処理
      *****************************************************************
       4101-AGE-CALC-SEC               SECTION.
      *
           IF  (   WRK-BIRTHDAY    <   SPA-SYSYMD )
               MOVE    WRK-BIRTHDAY    TO  WRK-CALC-YMD1
               MOVE    SPA-SYSYMD      TO  WRK-CALC-YMD2
           ELSE
               MOVE    SPA-SYSYMD      TO  WRK-CALC-YMD1
               MOVE    WRK-BIRTHDAY    TO  WRK-CALC-YMD2
           END-IF
      *
           COMPUTE WRK-AGE-YY          =   WRK-CALC-YY2
                                       -   WRK-CALC-YY1
           IF    ( WRK-CALC-YMD1(5:4)  >   WRK-CALC-YMD2(5:4) )
               COMPUTE WRK-AGE-YY      =   WRK-AGE-YY
                                       -   1
               COMPUTE WRK-AGE-MM      =  (WRK-CALC-MM2    +   12 )
                                       -   WRK-CALC-MM1
           ELSE
               COMPUTE WRK-AGE-MM      =   WRK-CALC-MM2
                                       -   WRK-CALC-MM1
           END-IF
      *
           IF    ( WRK-CALC-DD1        >   WRK-CALC-DD2 )
               COMPUTE WRK-AGE-MM      =   WRK-AGE-MM
                                       -   1
           END-IF
      *
           IF  (   WRK-BIRTHDAY    <   SPA-SYSYMD )
               CONTINUE
           ELSE
               COMPUTE WRK-AGE-YY  =   WRK-AGE-YY
                                   *   -1
               COMPUTE WRK-AGE-MM  =   WRK-AGE-MM
                                   *   -1
           END-IF
      *
           IF    ( WRK-AGE-YY          =   ZERO )
               MOVE    WRK-AGE-MM      TO  WRK-AGE-Z
               MOVE    "ヶ月"          TO  WRK-AGE-MEI
           ELSE
               MOVE    WRK-AGE-YY      TO  WRK-AGE-Z
               MOVE    "才"            TO  WRK-AGE-MEI
           END-IF
      *
           .
       4101-AGE-CALC-EXT.
           EXIT.
      *****************************************************************
      *    保険組み合わせコンボ設定処理
      *****************************************************************
       4101-CMB-HKNCOMBI-SET-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-HKNCOMBI
                                       SPA-KYOTUKEY
                                       SPA-KYOTU-SET
                                       ORCSHKNSETAREA
      *
           MOVE    WRK-KJNYMD          TO  SPA-SRYYMD
           MOVE    SPA-GMN-I01-NYUINKA-CD
                                       TO  SPA-SRYKA
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    SPA-NAI-I01-PTID    TO  SPA-PTID
           MOVE    SPA-NAI-I01-BIRTHDAY
                                       TO  SPA-BIRTHDAY
           MOVE    SPA-GMN-I01-HKNCOMBI-CD
                                       TO  SPA-HKNCOMBINUM
           IF    ( SPA-HKNCOMBINUM     =   "0999" )
               MOVE    SPACE           TO  SPA-HKNCOMBINUM
           END-IF
      *
           MOVE    "1"                 TO  ORCSHKN-KBN
      *
           CALL    "ORCSHKNSET"        USING   SPA-AREA
                                               ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               GO  TO  4101-CMB-HKNCOMBI-SET-EXT
           END-IF
      *
           IF    ( ORCSHKN-HKN-MAX     >   30 )
               MOVE    30              TO  SPA-GMN-I01-HKNCOMBI-CNT
           ELSE
               MOVE    ORCSHKN-HKN-MAX TO  SPA-GMN-I01-HKNCOMBI-CNT
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I01-HKNCOMBI-CNT )

               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX1)
                                       TO  SPA-GMN-I01-HKNCOMBI-ITM
                                                       (IDX1)(1 : 4)
               MOVE    ORCSHKN-GMN-FTNRATEMEI  (IDX1)
                                       TO  SPA-GMN-I01-HKNCOMBI-ITM2
                                                       (IDX1)
               MOVE    ORCSHKN-NAI-RSI-HKNKBN (IDX1)
                                       TO  SPA-GMN-I01-HKNCOMBI-ITM3
                                                       (IDX1)
      *        IF    ( ORCSHKN-NAI-HKNNUM (IDX1)   =   "975" )
      *            MOVE    "K"         TO  SPA-GMN-I01-HKNCOMBI-ITM3
      *                                                (IDX1)
      *        END-IF
               IF    ( ORCSHKN-NAI-HKNJANUM (IDX1) =   SPACE )
                   MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX1)
                                       TO  WRK-HKNCOMBI
               ELSE
                   MOVE    SPACE       TO  WRK-HKNCOMBI
                   STRING  ORCSHKN-GMN-HKNCOMBIMEI (IDX1)
                                               DELIMITED BY  "  "
                       " ("                    DELIMITED  BY  SIZE
                       ORCSHKN-NAI-HKNJANUM (IDX1)
                                               DELIMITED  BY  SPACE
                       ")"                     DELIMITED  BY  SIZE
                   INTO    WRK-HKNCOMBI
                   END-STRING
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    WRK-HKNCOMBI        TO  KANACHK-MAE-INPUT
                   MOVE    46                  TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   MOVE    KANACHK-MAE-INPUT
                                       TO  WRK-HKNCOMBI
               END-IF
               MOVE    WRK-HKNCOMBI    TO  SPA-GMN-I01-HKNCOMBI-ITM
                                                       (IDX1)(6 : )
      *
           END-PERFORM
      *
           IF    ( ORCSHKN-IDX    >=   1 )
            AND  ( ORCSHKN-IDX    <=   SPA-GMN-I01-HKNCOMBI-CNT )
               MOVE    SPA-GMN-I01-HKNCOMBI-ITM (ORCSHKN-IDX)
                                       TO  SPA-GMN-I01-HKNCOMBI
               MOVE    SPA-GMN-I01-HKNCOMBI-ITM2 (ORCSHKN-IDX)
                                       TO  SPA-NAI-I01-FTNRATE
               MOVE    SPA-GMN-I01-HKNCOMBI-ITM3 (ORCSHKN-IDX)
                                       TO  SPA-NAI-I01-RSI-HKNKBN
           END-IF
      *
           .
       4101-CMB-HKNCOMBI-SET-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴検索処理
      *****************************************************************
       4101-JYURRK-SEL-SEC           SECTION.
      *
           MOVE    SPACE           TO  WKJYURRK-REC
      *
      *    受診履歴検索
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM      TO  JYURRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID
                                   TO  JYURRK-PTID
           MOVE    WRK-KJNYMD      TO  JYURRK-SRYYMD
           MOVE    JYURRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"    TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           PERFORM
               UNTIL ( MCP-RC          NOT =   ZERO )
                OR   ( WKJYURRK-REC    NOT =   SPACE )
      *
               MOVE    MCPDATA-REC TO  JYURRK-REC  
      *
      *        最後の受診履歴を対象とする（診療日以前のもの）
               IF    ((SPA-GMN-I01-NYUINKA     NOT =   SPACE  )
                  AND (SPA-GMN-I01-NYUINKA-CD      =   JYURRK-SRYKA ) )
                OR    ( SPA-GMN-I01-NYUINKA        =   SPACE        )
                   MOVE    JYURRK-REC  TO  WKJYURRK-REC
               END-IF
      *
               MOVE    "tbl_jyurrk"    TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"    TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( WKJYURRK-REC        =   SPACE )
               GO  TO  4101-JYURRK-SEL-EXT
           END-IF
      *
           MOVE    WKJYURRK-REC        TO  JYURRK-REC 
      *
      *    入院科
           IF    ( SPA-GMN-I01-NYUINKA =   SPACE )
               MOVE    JYURRK-SRYKA    TO  WRK-CMB-CD
               PERFORM 4103-CMB-NYUINKA-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-NYUINKA
               MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I01-RECEKA
           END-IF
      *
      *    主治医
           IF    ( SPA-GMN-I01-DR (1)  =   SPACE )
               MOVE    JYURRK-DRCD (2 : 4) 
                                       TO  WRK-CMB-CD
               MOVE    1               TO  IDX1
               PERFORM 4103-CMB-DR-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-DR (1)
           END-IF
      *
           .
       4101-JYURRK-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理区分チェック処理
      *****************************************************************
       4100-SYORIKBN-CHK-SEC           SECTION.
      *
           MOVE    2               TO  SPA-GMN-I01-CUR
      *
           MOVE    I01-SYORIKBN    TO  SPA-GMN-I01-SYORIKBN
           PERFORM 4101-SYORIKBN-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4100-SYORIKBN-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  SPA-GMN-I01-IDOYMD
                                       SPA-NAI-I01-IDOYMD
                                       SPA-NAI-I01-IDOYMD-MAE
      *
      *    コラムリストの選択位置を退避
           MOVE    SPA-GMN-I01-NYUREKILST-SEL
                                   TO  WRK-NYUREKILST-SEL
      *
           INITIALIZE              SPA-GMN-I01-INPUT-AREA
                                   SPA-NAI-I01-INPUT-AREA
      *
      *    入院履歴マスタ検索処理（入力領域表示用）
           PERFORM 4101-PTNYUINRRK-SEL1-SEC
           IF    ( FLG-PTNYUINRRK  =   ZERO  )
      *        入力領域編集処理
               IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
                   PERFORM 4101-INPUT-AREA-EDIT2-SEC
               ELSE
                   PERFORM 4101-INPUT-AREA-EDIT-SEC
               END-IF
           END-IF
      *
      D    DISPLAY "FLG-PTNYUINRRK         =" FLG-PTNYUINRRK
      D    DISPLAY "SPA-GMN-I01-SYORIKBN-CD=" SPA-GMN-I01-SYORIKBN-CD
      D    DISPLAY "SPA-NAI-I01-MAEIDOYMD  =" SPA-NAI-I01-MAEIDOYMD
      D    DISPLAY "SPA-NAI-I01-PTRRKNUM   =" SPA-NAI-I01-PTRRKNUM
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO  )
            OR  (( SPA-GMN-I01-SYORIKBN-CD =   "01" )
             AND ( SPA-NAI-I01-MAEIDOYMD   =   SPACE ))
               PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *        保険組み合わせコンボ設定処理
               MOVE    SPA-SYSYMD      TO  WRK-KJNYMD
               PERFORM 4101-CMB-HKNCOMBI-SET-SEC
           END-IF
      *
      *    処理区分応じて画面編集の内容を変更する
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
      *    入院登録
           WHEN    "01"
      *
               IF    ( SPA-NAI-I01-MAEIDOYMD   =   SPACE )
      *            コンボボックス初期処理
                   MOVE    SPA-SYSYMD      TO  WRK-KJNYMD
                   PERFORM 310-CMB-SYOKI-SEC
      *
      *            受付マスタより入院科等を確定する
                   MOVE    SPA-SYSYMD      TO  WRK-KJNYMD
                   PERFORM 4101-UKETUKE-SEL-SEC
      *            受診履歴検索処理
                   MOVE    SPA-SYSYMD      TO  WRK-KJNYMD
                   PERFORM 4101-JYURRK-SEL-SEC
               ELSE
      *
                   MOVE    SPA-NAI-I01-MAEIDOYMD
                                       TO  WRK-KJNYMD
                   PERFORM 310-CMB-SYOKI-SEC
      *
                   MOVE    SPA-NAI-I01-MAEIDOYMD
                                       TO  WRK-KJNYMD
                   PERFORM 4101-CMB-HKNCOMBI-SET-SEC
      *
                   MOVE    SPA-NAI-I01-MAEIDOYMD
                                       TO  WRK-KJNYMD
                   PERFORM 4101-UKETUKE-SEL-SEC
                   MOVE    SPA-NAI-I01-MAEIDOYMD
                                       TO  WRK-KJNYMD
                   PERFORM 4101-JYURRK-SEL-SEC
      *
                   IF    ( SPA-NAI-I01-NYURRK-FLG   = ZERO )
                       PERFORM 4102-KIHON-CHK-SEC
                   END-IF
               END-IF
      *    退院登録
           WHEN    "02"
           WHEN    "11"
      *
      *        退院日にシステム日付か前回異動日で日付の新しい方を表示
               IF    ( SPA-SYSYMD      >   SPA-NAI-I01-MAEIDOYMD )
                   MOVE    SPA-SYSYMD      TO  WRK-YMD
               ELSE
                   MOVE    SPA-NAI-I01-MAEIDOYMD
                                           TO  WRK-YMD
               END-IF
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-TAIINYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I01-TAIINYMD
      *        コンボボックス初期処理
               MOVE    SPA-NAI-I01-TAIINYMD
                                       TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
      *
      *
      *    変更
           WHEN    "03"
               CONTINUE
      *    照会
           WHEN    "04"
               CONTINUE
      *    転科　転棟　転室
           WHEN    "08"
      *
      *        異動日にシステム日付か前回異動日で日付の新しい方を表示
               IF    ( SPA-SYSYMD      >   SPA-NAI-I01-MAEIDOYMD )
                   MOVE    SPA-SYSYMD      TO  WRK-YMD
               ELSE
                   MOVE    SPA-NAI-I01-MAEIDOYMD
                                           TO  WRK-YMD
               END-IF
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-IDOYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I01-IDOYMD
                                           SPA-NAI-I01-IDOYMD-MAE
      *
      *        コンボボックス初期処理
               MOVE    SPA-NAI-I01-IDOYMD
                                       TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
      *
      *        保険組み合わせコンボ設定処理
               MOVE    SPA-NAI-I01-IDOYMD
                                       TO  WRK-KJNYMD
               PERFORM 4101-CMB-HKNCOMBI-SET-SEC
      *
      *        受付マスタより入院科等を確定する
               MOVE    SPA-SYSYMD      TO  WRK-KJNYMD
               PERFORM 4101-UKETUKE-SEL-SEC
      *        受診履歴検索処理
               MOVE    SPA-NAI-I01-IDOYMD
                                       TO  WRK-KJNYMD
               PERFORM 4101-JYURRK-SEL-SEC
      *
               IF    ( SPA-NAI-I01-NYURRK-FLG   = ZERO )
                   PERFORM 4102-KIHON-CHK-SEC
               END-IF
      *
      *    照会
           WHEN    "10"
               CONTINUE
      *    その他
           WHEN    OTHER
               CONTINUE
           END-EVALUATE
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "10" )
               MOVE    WRK-NYUREKILST-SEL
                                       TO  SPA-GMN-I01-NYUREKILST-SEL
                                           SPA-GMN-I01-SELNUM
           ELSE
               IF    ( SPA-GMN-I01-NYUREKILST-CNT  >   ZERO )
                   MOVE    1           TO  SPA-GMN-I01-NYUREKILST-SEL
                                           SPA-GMN-I01-SELNUM
               END-IF
           END-IF
      *
      *    保険組合せが存在していること
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "01" )
               IF    ( SPA-GMN-I01-HKNCOMBI-CNT   >   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "1021"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-I01-CUR
                   GO  TO  4100-SYORIKBN-CHK-EXT
               END-IF
           END-IF
      *
           EVALUATE SPA-GMN-I01-SYORIKBN-CD
           WHEN    "04"
               MOVE    2               TO  SPA-GMN-I01-CUR
           WHEN    "10"
               MOVE    100             TO  SPA-GMN-I01-CUR
           WHEN    OTHER
               MOVE    4               TO  SPA-GMN-I01-CUR
           END-EVALUATE
      *
           .
      *
       4100-SYORIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理区分チェック処理
      *****************************************************************
       4101-SYORIKBN-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-I01-SYORIKBN    =   SPACE )
               MOVE    "1002"      TO  SPA-ERRCD
               GO  TO  4101-SYORIKBN-CHK-EXT
           END-IF
      *
      *    処理区分コンボチェック
           MOVE    SPA-GMN-I01-SYORIKBN-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SYORIKBN-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SYORIKBN
           ELSE
               MOVE    "0012"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-I01-CUR
               GO  TO  4101-SYORIKBN-CHK-EXT
           END-IF
      *
           .
      *
       4101-SYORIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    異動日チェック処理
      *****************************************************************
       4101-IDOYMD-CHK-SEC             SECTION.
      *
           MOVE    SPA-NAI-I01-IDOYMD
                                       TO  WRK-IDOYMD
      *
           IF    ( SPA-GMN-I01-IDOYMD  =   SPACE )
               IF    ( SPA-SYSYMD      >   SPA-NAI-I01-MAEIDOYMD )
                   MOVE    SPA-SYSYMD      TO  WRK-YMD
               ELSE
                   MOVE    SPA-NAI-I01-MAEIDOYMD
                                           TO  WRK-YMD
               END-IF
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-IDOYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I01-IDOYMD
           ELSE
               MOVE    SPA-GMN-I01-IDOYMD
                                       TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
      *
      *            エラーコードを設定（後続の処理は続行）
                   MOVE    "0013"      TO  SPA-ERRCD
                   MOVE    3           TO  SPA-GMN-I01-CUR
                   MOVE    WRK-IDOYMD  TO  WRK-YMD
                   PERFORM 5002-HIZUKE-CHK-SEC
               END-IF
      *
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-IDOYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I01-IDOYMD
           END-IF
      *
           IF    ( SPA-NAI-I01-IDOYMD  NOT =   SPA-NAI-I01-IDOYMD-MAE )
            OR   ( WRK-MCP-WIDGET          =   "SYORIKBN" )
               MOVE    SPA-NAI-I01-IDOYMD  TO  SPA-NAI-I01-IDOYMD-MAE
      *        コンボ初期処理
               MOVE    SPA-NAI-I01-IDOYMD
                                   TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
      *
      *        保険組み合わせコンボ初期処理
               PERFORM 4101-CMB-HKNCOMBI-SET-SEC
      *        受付マスタ検索処理
               MOVE    SPA-NAI-I01-IDOYMD
                                   TO  WRK-KJNYMD
               PERFORM 4101-UKETUKE-SEL-SEC
      *        受診履歴検索処理
               MOVE    SPA-NAI-I01-IDOYMD
                                   TO  WRK-KJNYMD
               PERFORM 4101-JYURRK-SEL-SEC
      *
      *        病室番号チェック
               MOVE    SPA-NAI-I01-IDOYMD
                                   TO  WRK-KIHON-KJNYMD
      *
               MOVE    SPA-NAI-I01-TOKNYUIN-ORG
                                   TO  WRK-TOKNYUINCD
               MOVE    SPA-GMN-I01-TOKHIGAITO-CD
                                   TO  WRK-TOKHIGAITO
               PERFORM 4101-BRMNUM-CHK-SEC
               MOVE    SPA-GMN-I01-BRMNUM
                                       TO  SPA-NAI-I01-BRMNUM-MAE
               MOVE    SPA-ERRCD       TO  SPA-NAI-I01-BRMNUM-ERR
      *
               MOVE    WRK-TOKNYUINCD  TO  WRK-CMB-CD
               PERFORM 4103-CMB-TOKNYUIN-SRH2-SEC
               IF    ( WRK-CMB-ITM     NOT =   SPACE )
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TOKNYUIN
                   MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I01-TOKNYUIN-ORG
                   MOVE    WRK-CMB-ITM4    TO  SPA-NAI-I01-TOKNYUINCD
                   MOVE    WRK-CMB-ITM5    TO  SPA-NAI-I01-TOKHIGAITOCD
               END-IF
      *
               MOVE    WRK-TOKHIGAITO      TO  SPA-GMN-I01-TOKHIGAITO-CD
               PERFORM 4101-TOKHIGAITO-CHK-SEC
      *
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-IDOYMD-CHK-EXT
               END-IF
      *
               MOVE    4               TO   SPA-GMN-I01-CUR
      *
           END-IF
      *
           .
      *
       4101-IDOYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院日チェック処理
      *****************************************************************
       4101-NYUINYMD-CHK-SEC           SECTION.
      *
           MOVE    SPA-NAI-I01-NYUINYMD    TO  WRK-NYUINYMD
      *
           IF    ( SPA-GMN-I01-NYUINYMD    =   SPACE )
      *
      *        入力がない場合、システム日付を設定
               MOVE    SPA-SYSYMD  TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-I01-NYUINYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-I01-NYUINYMD
           ELSE
               MOVE    SPA-GMN-I01-NYUINYMD
                                   TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
      *            エラーコードを設定（後続の処理は続行）
                   MOVE    "0017"      TO  SPA-ERRCD
                   MOVE    7           TO  SPA-GMN-I01-CUR
                   MOVE    WRK-NYUINYMD
                                       TO  WRK-YMD
                   PERFORM 5002-HIZUKE-CHK-SEC
               END-IF
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-NYUINYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I01-NYUINYMD
           END-IF
      *
           IF    (  SPA-NAI-I01-NYUINYMD
                                   NOT =   SPA-NAI-I01-NYUINYMD-MAE )
               MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  SPA-NAI-I01-NYUINYMD-MAE
      *        コンボ初期処理
               MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
      *
               MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  WRK-KJNYMD
      *        保険組み合わせコンボ初期処理
               PERFORM 4101-CMB-HKNCOMBI-SET-SEC
      *        受付マスタ検索処理
               MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  WRK-KJNYMD
               PERFORM 4101-UKETUKE-SEL-SEC
      *        受診履歴検索処理
               MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  WRK-KJNYMD
               PERFORM 4101-JYURRK-SEL-SEC
      *
               MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  WRK-KIHON-KJNYMD
      *        病室番号チェック
               PERFORM 4101-BRMNUM-CHK-SEC
               MOVE    SPA-GMN-I01-BRMNUM
                                       TO  SPA-NAI-I01-BRMNUM-MAE
               MOVE    SPA-ERRCD       TO  SPA-NAI-I01-BRMNUM-ERR
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-NYUINYMD-CHK-EXT
               END-IF
      *
               MOVE    25              TO   SPA-GMN-I01-CUR
      *
           END-IF
      *
           .
      *
       4101-NYUINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院日チェック処理
      *****************************************************************
       4101-TAIINYMD-CHK-SEC           SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-I01-TAIINYMD
      *
           IF    ( SPA-GMN-I01-TAIINYMD    =   SPACE )
               GO  TO  4101-TAIINYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-TAIINYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    "0021"      TO  SPA-ERRCD
               MOVE    11          TO  SPA-GMN-I01-CUR
               GO  TO  4101-TAIINYMD-CHK-EXT
           END-IF
      *
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-TAIINYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-I01-TAIINYMD
      *
           IF    ( WRK-MCP-WIDGET  =   "TAIINYMD" )
               MOVE    SPA-NAI-I01-TAIINYMD
                                   TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
           END-IF
      *
           .
      *
       4101-TAIINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院事由チェック処理
      *****************************************************************
       4101-TAIINCD-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-I01-TAIINCD =   SPACE )
               GO  TO  4101-TAIINCD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-TAIINCD-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-TAIINCD-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    "0028"          TO  SPA-ERRCD
               MOVE    20              TO  SPA-GMN-I01-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TAIINCD
           END-IF
      *
           .
      *
       4101-TAIINCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病室番号チェック処理
      *****************************************************************
       4101-BRMNUM-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I01-BRMNUM  =   SPACE )
               INITIALIZE  SPA-GMN-I01-CMB-BTUNAME
                           SPA-GMN-I01-BTUNAME
               GO  TO  4101-BRMNUM-CHK-EXT
           END-IF
      *
      *    全角半角混在チェック
           MOVE    SPA-GMN-I01-BRMNUM
                                   TO  WRK-KANACHK-MAE-INPUT
      *
           PERFORM     800-ENRTRY-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-I01-BRMNUM
           IF  ( FLG-ENTRYCHK      NOT =   1 )
               MOVE    SPACE           TO  SPA-GMN-I01-BTUNAME
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-I01-CUR
               GO  TO  4101-BRMNUM-CHK-EXT
           END-IF
      *
      *    左詰編集を行う
           MOVE    SPA-GMN-I01-BRMNUM  TO  WRK-BRMNUM-IN
           PERFORM 800-ORCSBRMNUM-SEC
           MOVE    WRK-BRMNUM-OT       TO  SPA-GMN-I01-BRMNUM
      *
      *    間に空白文字が混入していないかチェック
           MOVE    SPACE           TO  WRK-STR1
                                       WRK-STR2
           UNSTRING    SPA-GMN-I01-BRMNUM
               DELIMITED   BY  ALL SPACE
               INTO    WRK-STR1
                       WRK-STR2
           END-UNSTRING
      *
           IF    ( WRK-STR2        NOT =   SPACE )
               MOVE    SPACE           TO  SPA-GMN-I01-BTUNAME
               MOVE    "0014"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-I01-CUR
               GO  TO  4101-BRMNUM-CHK-EXT
           END-IF
      *
      *    病室番号存在チェック
           MOVE    WRK-KIHON-KJNYMD    TO  WRK-KJNYMD
           PERFORM 4101-BRMNUM-SEL-SEC
      *
           EVALUATE    SPA-GMN-I01-BTUNAME-CNT
      *    該当の病室が存在しなかった場合
           WHEN    0
               MOVE    SPACE           TO  SPA-GMN-I01-BTUNAME
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-I01-CUR
               GO  TO  4101-BRMNUM-CHK-EXT
      *    該当の病室が１件存在した場合
           WHEN    1
               MOVE    SPA-GMN-I01-BTUNAME-ITM (1)
                                       TO  SPA-GMN-I01-BTUNAME
               MOVE    SPA-GMN-I01-BTUNAME-ITM2 (1)
                                       TO  WRK-BRM-SAGAKU-CD
               MOVE    SPA-GMN-I01-BTUNAME-ITM3 (1)
                                       TO  WRK-BTU-KA-CD
               MOVE    SPA-GMN-I01-BTUNAME-ITM4 (1)
                                       TO  WRK-BRM-KA-CD
               MOVE    6               TO  SPA-GMN-I01-CUR
      *    該当の病室が複数件存在した場合
           WHEN    OTHER
               IF    ( SPA-GMN-I01-BTUNAME =   SPACE )
                   MOVE    SPA-GMN-I01-BTUNAME-ITM (1)
                                       TO  SPA-GMN-I01-BTUNAME
                   MOVE    SPA-GMN-I01-BTUNAME-ITM2 (1)
                                       TO  WRK-BRM-SAGAKU-CD
                   MOVE    SPA-GMN-I01-BTUNAME-ITM3 (1)
                                       TO  WRK-BTU-KA-CD
                   MOVE    SPA-GMN-I01-BTUNAME-ITM4 (1)
                                       TO  WRK-BRM-KA-CD
               ELSE
                   MOVE   SPA-GMN-I01-BTUNAME-CD
                                       TO  WRK-CMB-CD
                   PERFORM 4103-CMB-BTUNAME-CHK-SEC
                   IF    ( WRK-CMB-ITM =   SPACE )
                       MOVE    SPA-GMN-I01-BTUNAME-ITM (1)
                                       TO  SPA-GMN-I01-BTUNAME
                       MOVE    SPA-GMN-I01-BTUNAME-ITM2 (1)
                                       TO  WRK-BRM-SAGAKU-CD
                       MOVE    SPA-GMN-I01-BTUNAME-ITM3 (1)
                                       TO  WRK-BTU-KA-CD
                       MOVE    SPA-GMN-I01-BTUNAME-ITM4 (1)
                                       TO  WRK-BRM-KA-CD
                   ELSE
                      MOVE WRK-CMB-ITM TO  SPA-GMN-I01-BTUNAME
                      MOVE WRK-CMB-ITM2
                                       TO  WRK-BRM-SAGAKU-CD
                      MOVE WRK-CMB-ITM3
                                       TO  WRK-BTU-KA-CD
                      MOVE WRK-CMB-ITM4
                                       TO  WRK-BRM-KA-CD
                   END-IF
               END-IF
           END-EVALUATE
      *
      *    室料差額の初期値を設定
           IF    ( SPA-GMN-I01-BRMNUM      =  SPA-NAI-I01-BRMNUM-MAE )
               MOVE    SPA-GMN-I01-SAGAKU-CD
                                       TO  WRK-CMB-CD
               PERFORM 4103-CMB-SAGAKU-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-SAGAKU
           ELSE
               MOVE    WRK-BRM-SAGAKU-CD   TO  WRK-CMB-CD
               PERFORM 4103-CMB-SAGAKU-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-I01-SAGAKU
           END-IF
      *
      *    診療科の初期値を設定
           IF    ( SPA-GMN-I01-BRMNUM      =  SPA-NAI-I01-BRMNUM-MAE )
               MOVE    SPA-GMN-I01-NYUINKA-CD
                                           TO  WRK-CMB-CD
               PERFORM 4103-CMB-NYUINKA-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-I01-NYUINKA
               MOVE    WRK-CMB-ITM2        TO  SPA-NAI-I01-RECEKA
           ELSE
               IF    ( WRK-BRM-KA-CD       NOT =   SPACE )
                   MOVE    WRK-BRM-KA-CD   TO  WRK-CMB-CD
                   PERFORM 4103-CMB-NYUINKA-CHK-SEC
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-NYUINKA
                   MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I01-RECEKA
               ELSE
                       MOVE    WRK-BTU-KA-CD   TO  WRK-CMB-CD
                       PERFORM 4103-CMB-NYUINKA-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-NYUINKA
                       MOVE    WRK-CMB-ITM2 TO SPA-NAI-I01-RECEKA
               END-IF
           END-IF
      *
      *    特定入院料コンボ設定処理
           MOVE    WRK-KIHON-KJNYMD    TO  WRK-KJNYMD
           PERFORM 4101-CMB-TOKNYUIN-SET-SEC
      *
      *    特定入院料の初期値を設定
           MOVE    WRK-KIHON-KJNYMD    TO  WRK-KJNYMD
           PERFORM 4101-TOKNYUIN-SYOKI-SEC
      *
      *    入院料加算
           MOVE    WRK-KIHON-KJNYMD
                                       TO  WRK-KJNYMD
           PERFORM 4101-CMB-KSN-SET-SEC
      *
           .
      *
       4101-BRMNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病室検索処理
      *****************************************************************
       4101-BRMNUM-SEL-SEC             SECTION.
      *
           INITIALIZE              SYSKANRI-REC
                                   WRK-BTUJYOHO-TBL
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5002"          TO  SYS-KANRICD
           MOVE    WRK-KJNYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5002-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI   NOT =   ZERO )
                   OR    ( WRK-BTUJYOHO-CNT  >= 50  )
      *
               IF    ( SYS-5002-BRM-NUM
                                               =   SPA-GMN-I01-BRMNUM )
                   COMPUTE WRK-BTUJYOHO-CNT    
                       =   WRK-BTUJYOHO-CNT    +   1
                   MOVE    SYS-5002-KBNCD (1 : 2)
                                               TO  WRK-BTUJY-BTUNUM
                                                   (WRK-BTUJYOHO-CNT)
                   MOVE    SYS-5002-BRM-SAGAKU
                                               TO  WRK-BTUJY-BRM-SAGAKU
                                                   (WRK-BTUJYOHO-CNT)
                   MOVE    SYS-5002-BRM-KA
                                               TO  WRK-BTUJY-BRM-KA
                                                   (WRK-BTUJYOHO-CNT)
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5002-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    病棟名検索処理
           PERFORM 4101-BTUNAME-SEL-SEC
      *
           .
       4101-BRMNUM-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟名称設定処理
      *****************************************************************
       4101-BTUNAME-SEL-SEC            SECTION.
      *
      *
           INITIALIZE              SYSKANRI-REC
                                   SPA-GMN-I01-CMB-BTUNAME
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI            NOT = ZERO )
                   OR    ( SPA-GMN-I01-BTUNAME-CNT    >= 50  )
      *
               PERFORM VARYING IDX-BTUNUM  FROM    1   BY  1
                       UNTIL ( IDX-BTUNUM  >   WRK-BTUJYOHO-CNT )
      *
                   IF    ( WRK-BTUJY-BTUNUM (IDX-BTUNUM)
                                   =   SYS-5001-BTU-NUM )
      *
      *
                       COMPUTE SPA-GMN-I01-BTUNAME-CNT
                           =   SPA-GMN-I01-BTUNAME-CNT +   1
      *
                       MOVE    SYS-5001-BTU-NUM
                               TO  SPA-GMN-I01-BTUNAME-ITM
                                    (SPA-GMN-I01-BTUNAME-CNT) (1 : 2)
                       MOVE    SYS-5001-BTU-NAME
                               TO  SPA-GMN-I01-BTUNAME-ITM
                                    (SPA-GMN-I01-BTUNAME-CNT) (4 : )
                       MOVE    WRK-BTUJY-BRM-SAGAKU (IDX-BTUNUM)
                               TO  SPA-GMN-I01-BTUNAME-ITM2
                                    (SPA-GMN-I01-BTUNAME-CNT)
                       MOVE    SYS-5001-BTU-KA
                               TO  SPA-GMN-I01-BTUNAME-ITM3
                                    (SPA-GMN-I01-BTUNAME-CNT)
                       MOVE    WRK-BTUJY-BRM-KA (IDX-BTUNUM)
                               TO  SPA-GMN-I01-BTUNAME-ITM4
                                    (SPA-GMN-I01-BTUNAME-CNT)
      *
                       MOVE    WRK-BTUJYOHO-CNT
                               TO  IDX-BTUNUM
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4101-BTUNAME-SEL-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボ設定処理
      *****************************************************************
       4101-CMB-TOKNYUIN-SET-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-I01-CMB-TOKNYUIN
                       SPA-NAI-I01-KHNRYO-G
                       SPA-NAI-I01-BTUSBT
                       SPA-NAI-I01-BTUKHNKBNCD
                       SPA-NAI-I01-BRMKHNKBNCD
                       SPA-NAI-I01-BTUTOKCD
                       SPA-NAI-I01-BRMTOKCD
                       SPA-NAI-I01-SELNYUKHN
                       SPA-GMN-I01-CMB-TOKHIGAITO
      *
           MOVE    WRK-KJNYMD  TO  WRK-AGECHK-SRYYMD
           PERFORM 4801-RJN-TAISYO-CHK-SEC
      *
      *    病棟情報からコンボ項目を設定
           MOVE    "5001"          TO  WRK-KANRICD
           MOVE    SPA-GMN-I01-BTUNAME-CD
                                   TO  WRK-KBNCD
           MOVE    WRK-KJNYMD      TO  WRK-YUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
               MOVE    SYS-5001-BTU-SBT    TO  SPA-NAI-I01-BTUSBT
               MOVE    SYS-5001-KHNKBNCD   TO  SPA-NAI-I01-BTUKHNKBNCD
               MOVE    "2"                 TO  WRK-CMB-CD(1:1)
               MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                           TO  WRK-CMB-CD(2:2)
               IF    ( WRK-KJNYMD      >=  CONST-H181001 )
                   PERFORM 41011-TOKNYUIN-SYOKI-SEC
                   MOVE    WRK-CMB-CD      TO  SPA-NAI-I01-BTUTOKCD
               END-IF
      *
      *        病棟の入院基本料を退避する
               IF    ( SYS-5001-BTU-KHNSRYCD
                               NOT =   SPACE )
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                   TO  SPA-NAI-I01-KHNRYO-CD
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                   TO  WRK-SRYCD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU   =   ZERO )
                       MOVE    TNS-NAME    TO   SPA-NAI-I01-KHNRYO-NM
                   END-IF
               END-IF
      *
           IF    ( SPA-NAI-I01-BTUSBT  =   CONST-KANSATU )
               PERFORM 41011-KANSATU-DAT-SET-SEC
           ELSE
               PERFORM 41011-CMB-TOKNYUIN-SET-SEC
           END-IF
      *
           PERFORM 310-CMB-TOKHIGAITO-SET-SEC
      *
           .
       4101-CMB-TOKNYUIN-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボ設定処理
      *****************************************************************
       41011-CMB-TOKNYUIN-SET-SEC      SECTION.
      *
               IF    ( WRK-KJNYMD         >=   CONST-H180701 )
                AND  ( SYS-5001-BTU-SBT    =   "04" OR "09" )
                   IF    ( SYS-5001-BTU-SBT    =   "04" )
                       MOVE    1           TO  FLG-RYOYOKHN
                   ELSE
                       MOVE    2           TO  FLG-RYOYOKHN
                   END-IF
                   PERFORM 410111-RYOYOKHN-EDIT-SEC
               END-IF
      *
               IF    ( WRK-KJNYMD         >=   CONST-H280401 )
                AND  ( SYS-5001-BTU-SBT    =   "04" )
      *
                   INITIALIZE                  SS014-AREA
                   MOVE    WRK-KJNYMD      TO  SS014-SRYYMD
                   CALL    "ORCSS014"      USING
                                           SS014-AREA
                                           SPA-AREA
                   IF    ( SS014-SSTKJN-CD (8015)  =   "1" )
                       PERFORM 410111-RYOYOIPN-EDIT-SEC
                   END-IF
               END-IF
      *
               IF    ( SYS-5001-BTU-TOKTEI-NYUIN   NOT =   SPACE )
                   EVALUATE    TRUE
                   WHEN  ( SYS-5001-BTU-TOKTEI-NYUIN
                               =   "01" OR "02" OR "14"
                                OR "17" OR "26" )
                       PERFORM 41011-PT-AGE-SEC
                       EVALUATE    TRUE
                       WHEN  ( WRK-KJNYMD >=   CONST-H280401 )
                         AND ( WRK-NENREI  <   20 )
                       WHEN  ( WRK-KJNYMD  <   CONST-H280401 )
                         AND ( WRK-NENREI  <   15 )
                           MOVE    "5110"      TO  WRK-KANRICD
                           MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                               TO  WRK-KBNCD
                           MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       END-EVALUATE
                   WHEN    OTHER
                       MOVE    "5110"      TO  WRK-KANRICD
                       MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                           TO  WRK-KBNCD
                       MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                   END-EVALUATE
      *
                   IF    ( WRK-KJNYMD         >=   CONST-H280401 )
                       IF    ( SYS-5001-BTU-TOKTEI-NYUIN   =   "03" )
                           MOVE    "5110"      TO  WRK-KANRICD
                           MOVE    "49"        TO  WRK-KBNCD
                           MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                           MOVE    "5110"      TO  WRK-KANRICD
                           MOVE    "50"        TO  WRK-KBNCD
                           MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       END-IF
                       IF    ( SYS-5001-BTU-TOKTEI-NYUIN   =   "04" )
                           MOVE    "5110"      TO  WRK-KANRICD
                           MOVE    "51"        TO  WRK-KBNCD
                           MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                           MOVE    "5110"      TO  WRK-KANRICD
                           MOVE    "52"        TO  WRK-KBNCD
                           MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       END-IF
                   END-IF
      *
                   IF    ( SYS-5001-BTU-TOKTEI-NYUIN   =   "11" )
                    AND  ( SPA-GMN-I01-SYORIKBN-CD NOT =   "01" )
                       MOVE    "5110"      TO  WRK-KANRICD
                       MOVE    "24"        TO  WRK-KBNCD
                       MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                   END-IF
      *
                   IF    ( SYS-5001-BTU-TOKTEI-NYUIN   =   "12" )
                    AND  ( SPA-GMN-I01-SYORIKBN-CD NOT =   "01" )
                       MOVE    "5110"      TO  WRK-KANRICD
                       MOVE    "25"        TO  WRK-KBNCD
                       MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                   END-IF
      *
               END-IF
      *
      *    病室情報からコンボ項目を設定
           MOVE    "5002"          TO  WRK-KANRICD
           MOVE    SPA-GMN-I01-BTUNAME-CD
                                   TO  WRK-KBNCD (1 : 2)
           MOVE    SPA-GMN-I01-BRMNUM
                                   TO  WRK-BRMNUM
           PERFORM 4800-BRMNUM-EDIT-SEC
           MOVE    WRK-BRMNUM-EDT  TO  WRK-KBNCD (3 : )
           MOVE    WRK-KJNYMD      TO  WRK-YUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    SYSKANRI-REC TO  SYS-5002-REC
      *
               MOVE    SYS-5002-KHNKBNCD   TO  SPA-NAI-I01-BRMKHNKBNCD
               MOVE    "3"                 TO  WRK-CMB-CD(1:1)
               MOVE    SYS-5002-BRM-TOKTEI-NYUIN
                                           TO  WRK-CMB-CD(2:2)
               IF    ( WRK-KJNYMD      >=  CONST-H181001 )
                   PERFORM 41011-TOKNYUIN-SYOKI-SEC
                   MOVE    WRK-CMB-CD      TO  SPA-NAI-I01-BRMTOKCD
               END-IF

      *
               IF    ( SYS-5002-BRM-TOKTEI-NYUIN
                               NOT =   SPACE )
                   EVALUATE    TRUE
                   WHEN    ( SYS-5002-BRM-TOKTEI-NYUIN   =   "02" )
      *
                       MOVE    "5111"      TO  WRK-KANRICD
                       MOVE    "02"        TO  WRK-KBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
      *
                       MOVE    "5111"      TO  WRK-KANRICD
                       MOVE    "23"        TO  WRK-KBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                   WHEN    ( SYS-5002-BRM-TOKTEI-NYUIN = "18" OR "54" )
                       AND ( WRK-KJNYMD   >=  CONST-H240401 )
      *
                       MOVE    "5111"      TO  WRK-KANRICD
                       MOVE    "18"        TO  WRK-KBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
      *
                       MOVE    "5111"      TO  WRK-KANRICD
                       MOVE    "54"        TO  WRK-KBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                   WHEN    ( SYS-5002-BRM-TOKTEI-NYUIN = "92" OR "93" )
                       AND ( WRK-KJNYMD   >=  CONST-H240401 )
      *
                       MOVE    "5111"      TO  WRK-KANRICD
                       MOVE    "92"        TO  WRK-KBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
      *
                       MOVE    "5111"      TO  WRK-KANRICD
                       MOVE    "93"        TO  WRK-KBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                   WHEN    OTHER
                       MOVE    "5111"      TO  WRK-KANRICD
                       MOVE    SYS-5002-BRM-TOKTEI-NYUIN
                                           TO  WRK-KBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                   END-EVALUATE
      *
                   IF    ( WRK-KJNYMD     >=  CONST-H280401 )
                       IF    ( SYS-5002-BRM-TOKTEI-NYUIN   =   "01" )
                           MOVE    "5111"      TO  WRK-KANRICD
                           MOVE    "BT"        TO  WRK-KBNCD
                           MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                           MOVE    "5111"      TO  WRK-KANRICD
                           MOVE    "BU"        TO  WRK-KBNCD
                           MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( WRK-KJNYMD     >=  CONST-H280401 )
               IF    ( SPA-NAI-I01-BTUSBT  =   "05" )
                   IF    ( SYS-5001-BTU-KHNSRYCD   =   "190126310" )
                       MOVE    "5111"      TO  WRK-KANRICD
                       MOVE    "BK"        TO  WRK-KBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       MOVE    "5111"      TO  WRK-KANRICD
                       MOVE    "BL"        TO  WRK-KBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                   END-IF
                   IF    ( SYS-5001-BTU-KHNSRYCD   =   "190093710" )
                       MOVE    "5111"      TO  WRK-KANRICD
                       MOVE    "BM"        TO  WRK-KBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       MOVE    "5111"      TO  WRK-KANRICD
                       MOVE    "BN"        TO  WRK-KBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                   END-IF
                   IF    ( SYS-5001-BTU-KHNSRYCD   =   "190093810" )
                       MOVE    "5111"      TO  WRK-KANRICD
                       MOVE    "BO"        TO  WRK-KBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       MOVE    "5111"      TO  WRK-KANRICD
                       MOVE    "BP"        TO  WRK-KBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                   END-IF
                   IF    ( SYS-5001-BTU-KHNSRYCD   =   "190093910" )
                       MOVE    "5111"      TO  WRK-KANRICD
                       MOVE    "BQ"        TO  WRK-KBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       MOVE    "5111"      TO  WRK-KANRICD
                       MOVE    "BR"        TO  WRK-KBNCD
                       MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                       PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF    ( WRK-KJNYMD     >=  CONST-H240401 )
               EVALUATE  TRUE
               WHEN  ( SPA-NAI-I01-BTUSBT  =   "08" )
                AND  ( SYS-5001-CRYOYOKBN  =   "1" )
                   MOVE    4         TO  FLG-RYOYOKHN
                   PERFORM 410111-RYOYOKHN-EDIT-SEC
               WHEN  ( SPA-NAI-I01-BTUSBT  =   "09" )
                   MOVE    "5110"      TO  WRK-KANRICD
                   MOVE    "2"         TO  WRK-TOKNYUIN-KBN
                   EVALUATE    TRUE
                   WHEN  ( WRK-KJNYMD     >=  CONST-H260401 )
                       EVALUATE    SYS-5001-CRYOYOKBN
                       WHEN    "1"
                           MOVE    "38"  TO  WRK-KBNCD
                           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       WHEN    "2"
                           MOVE    "39"  TO  WRK-KBNCD
                           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       WHEN    "3"
                           MOVE    "40"  TO  WRK-KBNCD
                           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       WHEN    "4"
                           MOVE    "35"  TO  WRK-KBNCD
                           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       WHEN    "5"
                           MOVE    "36"  TO  WRK-KBNCD
                           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       WHEN    "6"
                           MOVE    "37"  TO  WRK-KBNCD
                           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       END-EVALUATE
                   WHEN    OTHER
                       EVALUATE    SYS-5001-CRYOYOKBN
                       WHEN    "1"
                           MOVE    "35"  TO  WRK-KBNCD
                           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       WHEN    "2"
                           MOVE    "36"  TO  WRK-KBNCD
                           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       WHEN    "3"
                           MOVE    "37"  TO  WRK-KBNCD
                           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
                       END-EVALUATE
                   END-EVALUATE
               WHEN  ( SYS-5001-BTU-TOKTEI-NYUIN
                                     =  "33" OR "34" )
                   MOVE    3         TO  FLG-RYOYOKHN
                   PERFORM 410111-RYOYOKHN-EDIT-SEC
               WHEN  ( SPA-NAI-I01-BTUSBT  =   "01" )
                AND  ( SYS-5001-CRYOYOKBN  =   "1" )
                   IF    ( WRK-KJNYMD     >=  CONST-H241001 )
                       MOVE    5         TO  FLG-RYOYOKHN
                       PERFORM 410111-RYOYOKHN-EDIT-SEC
                   END-IF
               WHEN  ( SPA-NAI-I01-BTUSBT  =   "06" )
                AND  ( SYS-5001-CRYOYOKBN  =   "1" )
                   IF    ( WRK-KJNYMD     >=  CONST-H261001 )
                       MOVE    5         TO  FLG-RYOYOKHN
                       PERFORM 410111-RYOYOKHN-EDIT-SEC
                   END-IF
               END-EVALUATE
           END-IF
      *
      *    老人の場合、老人一般病棟特定入院基本料をコンボに設定
      *    平成２２年４月から年齢要件廃止
           MOVE    WRK-KJNYMD  TO  WRK-AGECHK-SRYYMD
           PERFORM 4801-RJN-TAISYO-CHK-SEC
      *
           MOVE    ZERO        TO  FLG-EDIT
           IF  ( WRK-KJNYMD   >=  CONST-H220401 )
      *
               MOVE    1       TO  FLG-EDIT
      *
               IF    ( WRK-KJNYMD   >=  CONST-H240401 )
                   IF    ( SPA-NAI-I01-BTUSBT  =   "01" OR "05" OR "06")
                       IF    ( SYS-5001-BTU-TOKTEI-NYUIN
                                           =   "33" OR "34" )
                           MOVE    ZERO    TO  FLG-EDIT
                       END-IF
                   END-IF
               END-IF
      *
               IF  ( WRK-KJNYMD   >=  CONST-H241001 )
                   IF    ( SPA-NAI-I01-BTUSBT  =   "01" )
                     AND ( SYS-5001-CRYOYOKBN  =   "1" )
                           MOVE    ZERO    TO  FLG-EDIT
                   END-IF
               END-IF
           ELSE
               IF  ( FLG-RJN   =   1 )
                           MOVE    1       TO  FLG-EDIT
               END-IF
           END-IF
      *
           IF    ( FLG-EDIT            =   1 )
               IF    ( WRK-KJNYMD     >=  CONST-H261001 )
                   IF    ( SPA-NAI-I01-BTUSBT  =   "05" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-EDIT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( FLG-EDIT            =   1 )
      *
               MOVE    "5111"      TO  WRK-KANRICD
               MOVE    "22"        TO  WRK-KBNCD
               MOVE    "3"         TO  WRK-TOKNYUIN-KBN
               PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
      *
               IF    ( WRK-KJNYMD   >=  CONST-H260401 )
                AND  ( SPA-NAI-I01-BTUSBT  =   "05" OR "06")
                   CONTINUE
               ELSE
                   MOVE    "5111"      TO  WRK-KANRICD
                   MOVE    "24"        TO  WRK-KBNCD
                   MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                   PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
               END-IF
           END-IF
      *
      *    短期滞在手術基本料をコンボに設定
           MOVE    "5111"      TO  WRK-KANRICD
           MOVE    "21"        TO  WRK-KBNCD
           MOVE    "3"         TO  WRK-TOKNYUIN-KBN
           PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
      *
           IF    ( WRK-KJNYMD  >=  CONST-H260401 )
               PERFORM 410111-TANKI3-EDIT-SEC
           ELSE
               MOVE    "5111"      TO  WRK-KANRICD
               MOVE    "52"        TO  WRK-KBNCD
               MOVE    "3"         TO  WRK-TOKNYUIN-KBN
               PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
           END-IF
      *
      *    入院履歴に登録のある特定入院料がコンボボックスにない場合は、
      *    追加表示を行う。
           IF    ( SPA-NAI-I01-TOKUTEI-KBN
                                   =   "2" OR "3" )
            AND  ( SPA-GMN-I01-BTUNAME-CD
                                   =   SPA-NAI-I01-BTUNAME-MOT (1 : 2))
            AND  ( SPA-GMN-I01-BRMNUM
                                   =   SPA-NAI-I01-BRMNUM-MOT )
            AND  ( SPA-GMN-I01-SYORIKBN-CD
                               NOT =   "08" )
               MOVE    SPACE           TO  WRK-CMB-CD
               MOVE    SPA-NAI-I01-TOKUTEI-KBN
                                       TO  WRK-CMB-CD (1 : 1)
               MOVE    SPA-NAI-I01-TOKUTEI-NYUIN
                                       TO  WRK-CMB-CD (2 : 2)
               PERFORM 4103-CMB-TOKNYUIN-SRH2-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   IF    ( SPA-NAI-I01-TOKUTEI-KBN =   "2" )
                       MOVE    "5110"  TO  WRK-KANRICD
                   ELSE
                       MOVE    "5111"  TO  WRK-KANRICD
                   END-IF
                   MOVE    SPA-NAI-I01-TOKUTEI-NYUIN
                                       TO  WRK-KBNCD
                   MOVE    SPA-NAI-I01-TOKUTEI-KBN
                                       TO  WRK-TOKNYUIN-KBN
                   PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
               END-IF
           END-IF
      *
           .
       41011-CMB-TOKNYUIN-SET-EXT.
           EXIT.
      *****************************************************************
      *    短期滞在手術等基本料３編集処理
      *****************************************************************
       410111-TANKI3-EDIT-SEC       SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5003"          TO  SYS-KANRICD
           MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5003-REC
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               IF    ( SYS-5003-KBNCD  NOT =   "*" )
                   MOVE    "5111"  TO  WRK-KANRICD
                   MOVE    SYS-5003-TOKCD
                                   TO  WRK-KBNCD
                   MOVE    "3"     TO  WRK-TOKNYUIN-KBN
                   PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5003-REC
      *
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       410111-TANKI3-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    療養病棟入院基本料編集処理
      *****************************************************************
       410111-RYOYOKHN-EDIT-SEC      SECTION.
      *
           MOVE    1               TO  SPA-NAI-I01-SELNYUKHN
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5111"          TO  SYS-KANRICD
           MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY23-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5111-REC
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    ZERO        TO  FLG-TARGET
      *
               IF    ( FLG-RYOYOKHN                =   1 )
                AND  ( SYS-5111-BRM-NYUINRYOKBN    =   "05" )
                       IF    ( FLG-LIFE-RJN    =   ZERO OR 2)
                           IF    ( SYS-5111-SEIKATU-RYOKBN
                                               =   SPACE )
                               MOVE    1   TO  FLG-TARGET
                           END-IF
                       END-IF
                       IF    ( FLG-LIFE-RJN    =   1    OR 2)
                           IF    ( SYS-5111-SEIKATU-RYOKBN
                                           NOT =   SPACE )
                               MOVE    1   TO  FLG-TARGET
                           END-IF
                       END-IF
               END-IF
      *
               IF    ( FLG-RYOYOKHN                =   3 )
                AND  ( SYS-5111-BRM-NYUINRYOKBN    =   "05" )
                  IF    ( WRK-KJNYMD             >=   CONST-H260401 )
                          IF    ( SYS-5111-SEIKATU-RYOKBN
                                              =   SPACE )
                              MOVE    1   TO  FLG-TARGET
                          END-IF
                  ELSE
                       IF    ( FLG-LIFE-RJN    =   ZERO OR 2)
                           IF    ( SYS-5111-SEIKATU-RYOKBN
                                               =   SPACE )
                               MOVE    1   TO  FLG-TARGET
                           END-IF
                       END-IF
                       IF    ( FLG-LIFE-RJN    =   1    OR 2)
                           IF    ( SYS-5111-SEIKATU-RYOKBN
                                           NOT =   SPACE )
                               MOVE    1   TO  FLG-TARGET
                           END-IF
                       END-IF
                  END-IF
               END-IF
      *
               IF    ( FLG-RYOYOKHN                =   5 )
                AND  ( SYS-5111-BRM-NYUINRYOKBN    =   "05" )
                   IF    ( SYS-5111-SEIKATU-RYOKBN
                                               =   SPACE )
                       MOVE    1   TO  FLG-TARGET
                   END-IF
               END-IF
      *
               IF    ( FLG-RYOYOKHN                =   2 OR 4)
                AND  ( SYS-5111-BRM-NYUINRYOKBN    =   "06" )
                   IF    ( FLG-LIFE-RJN        =   ZERO OR 2)
                       IF    ( SYS-5111-SEIKATU-RYOKBN
                                                   =   SPACE )
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   END-IF
                   IF    ( FLG-LIFE-RJN        =   1    OR 2)
                       IF    ( SYS-5111-SEIKATU-RYOKBN
                                               NOT =   SPACE )
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( WRK-KJNYMD  >=  CONST-H220401 )
                   IF    ( SYS-5111-SRYCD
                                       =   "099999953"
                                        OR "099999954" )
                           MOVE  ZERO  TO  FLG-TARGET
                   END-IF
                   IF   (( FLG-RYOYOKHN    =   1 )
                    AND  ( SYS-5001-KHNSRYCD
                                       =   SPACE
                                  OR       "099999953"
                                  OR       "190121310"
                                  OR       "190121410"
                                  OR       "190121510"
                                  OR       "190121610"
                                  OR       "190121710"
                                  OR       "190131610"
                                  OR       "190131810"
                                  OR       "190132010"
                                  OR       "190132210"
                                  OR       "190121810" ))
                    OR   ( FLG-RYOYOKHN    =   3 OR 5 )
                       IF    ( SYS-5111-SRYCD
                                       =   "190121310"
                                  OR       "190121410"
                                  OR       "190121510"
                                  OR       "190121610"
                                  OR       "190121710"
                                  OR       "190131610"
                                  OR       "190131810"
                                  OR       "190132010"
                                  OR       "190132210"
                                  OR       "190123710"
                                  OR       "190123810"
                                  OR       "190123910"
                                  OR       "190124010"
                                  OR       "190124110"
                                  OR       "190131710"
                                  OR       "190131910"
                                  OR       "190132110"
                                  OR       "190132310"
                                  OR       "190121810"
                                  OR       "190124210" )
                           CONTINUE
                       ELSE
                           MOVE  ZERO  TO  FLG-TARGET
                       END-IF
                   END-IF
                   IF   (( FLG-RYOYOKHN    =   1 )
                    AND  ( SYS-5001-KHNSRYCD
                                       =   "099999954"
                                  OR       "190132410"
                                  OR       "190132610"
                                  OR       "190132810"
                                  OR       "190133010"
                                  OR       "190133210"
                                  OR       "190133410"
                                  OR       "190133610"
                                  OR       "190133810"
                                  OR       "190134010"
                                  OR       "190121810" ))
                       IF    ( SYS-5111-SRYCD
                                       =   "190132410"
                                  OR       "190132610"
                                  OR       "190132810"
                                  OR       "190133010"
                                  OR       "190133210"
                                  OR       "190133410"
                                  OR       "190133610"
                                  OR       "190133810"
                                  OR       "190134010"
                                  OR       "190132510"
                                  OR       "190132710"
                                  OR       "190132910"
                                  OR       "190133110"
                                  OR       "190133310"
                                  OR       "190133510"
                                  OR       "190133710"
                                  OR       "190133910"
                                  OR       "190134110"
                                  OR       "190121810"
                                  OR       "190124210" )
                           CONTINUE
                       ELSE
                           MOVE  ZERO  TO  FLG-TARGET
                       END-IF
                   END-IF
                   IF   (  FLG-RYOYOKHN    =   3 OR 5 )
                       IF    ( SYS-5111-SRYCD
                                  =        "190121810"
                                  OR       "190124210" )
                           MOVE  ZERO  TO  FLG-TARGET
                       END-IF
                   END-IF
                   IF   (  FLG-RYOYOKHN    =   4 )
                       IF    ( SYS-5111-SRYCD
                                  =        "190122510"
                                  OR       "190124810" )
                           MOVE  ZERO  TO  FLG-TARGET
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( FLG-TARGET  =   1 )
      *
                   MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM
                                           TO  WRK-TOKNYUIN-NM
                   MOVE    "3"             TO  WRK-TOKNYUIN-KBN
                   MOVE    SYS-5111-KBNCD
                                           TO  WRK-TOKNYUIN-CD
                   MOVE    SYS-5111-BRM-NYUINRYOKBN
                                   TO  WRK-TOKNYUIN-NYUINRYOKBN
                   MOVE    SYS-5111-SRYCD
                                       TO  WRK-TOKNYUIN-SRYCD-X
                   PERFORM 4101-CMB-TOKNYUIN-DAT-SET-SEC
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY23-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5111-REC
      *
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       410111-RYOYOKHN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    療養・一般病棟入院基本料編集処理
      *****************************************************************
       410111-RYOYOIPN-EDIT-SEC      SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5111"          TO  SYS-KANRICD
           MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY23-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5111-REC
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    1           TO  FLG-TARGET
      *
               IF    ( SYS-5111-BRM-NYUINRYOKBN    =   "12" )
                   CONTINUE
               ELSE
                   MOVE    ZERO    TO  FLG-TARGET
               END-IF
      *
               IF    ( FLG-TARGET  =   1 )
                   MOVE    SYS-5111-KANRICD
                                       TO  WRK-KANRICD
                   MOVE    SYS-5111-KBNCD
                                       TO  WRK-KBNCD
                   MOVE    "3"         TO  WRK-TOKNYUIN-KBN
                   PERFORM 4100-CMB-TOKNYUIN-DAT-SET-SEC
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY23-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5111-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       410111-RYOYOIPN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算コンボ初期設定処理
      *****************************************************************
       4101-CMB-KSN-SET-SEC            SECTION.
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   CONST-NYUIN
                                            OR CONST-IDO    )
               PERFORM 41011-CMB-KSN-SET-SEC
           ELSE
      *
               INITIALIZE                  SPA-GMN-I01-CMB-KSN
                                           SPA-GMN-I01-KSN-TBL
      *
           END-IF
      *
           .
       4101-CMB-KSN-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算コンボ初期設定処理
      *****************************************************************
       41011-CMB-KSN-SET-SEC            SECTION.
      *
           PERFORM VARYING IDX-KSN     FROM    1   BY  1
                   UNTIL ( IDX-KSN     >   CONST-GMN-KSN-MAX )
               MOVE    SPA-GMN-I01-KSN-SRYCD (IDX-KSN)
                                       TO  WRK-KSN-SRYCD (IDX-KSN)
               MOVE    SPA-GMN-I01-KSN-CD    (IDX-KSN)
                                       TO  WRK-KSN-CD    (IDX-KSN)
           END-PERFORM
      *
           INITIALIZE                      SPA-GMN-I01-CMB-KSN
                                           SPA-GMN-I01-KSN-TBL
                                           SPA-I01-JOKUSOUYMD
                                           SPA-I01-JOKUSOUKBN
      *
           MOVE    1                   TO  SPA-GMN-I01-CMB-KSN-EDITABLE
      *
           INITIALIZE                      SI01-AREA
           MOVE    SPA-NAI-I01-PTID    TO  SI01-PTID
           MOVE    WRK-KJNYMD          TO  SI01-SRYYMD
           MOVE    SPA-NAI-I01-NYUINYMD TO SI01-NYUINYMD
           MOVE    SPA-GMN-I01-BTUNAME-CD
                                       TO  SI01-BTUNUM
           MOVE    SPA-NAI-I01-BTUSBT  TO  SI01-BTUSBT
           MOVE    SPA-NAI-I01-SNTNYUINCD
                                       TO  SI01-SNTNYUINCD
           CALL    "ORCSI01"           USING
                                       SI01-AREA
                                       SPA-AREA
           MOVE    SI01-JOKUSOUYMD     TO  SPA-I01-JOKUSOUYMD
           MOVE    SI01-JOKUSOUKBN     TO  SPA-I01-JOKUSOUKBN
      *
           PERFORM VARYING IDX-KSN     FROM    1   BY  1
                   UNTIL ( IDX-KSN     >   SI01-KSN-MAX )
      *
               MOVE    SI01-KSN-NAME (IDX-KSN)
                                   TO  SPA-GMN-I01-LBLKSN (IDX-KSN)
               MOVE    SI01-KSN-SRYCD (IDX-KSN)
                                   TO  SPA-GMN-I01-KSN-SRYCD (IDX-KSN)
               MOVE    "1"         TO  SPA-GMN-I01-KSNL      (IDX-KSN 1)
               MOVE    "算定しない"
                                   TO  SPA-GMN-I01-KSNMEIL   (IDX-KSN 1)
               MOVE    "2"         TO  SPA-GMN-I01-KSNL      (IDX-KSN 2)
               MOVE    "算定する"
                                   TO  SPA-GMN-I01-KSNMEIL   (IDX-KSN 2)
               MOVE    2           TO  SPA-GMN-I01-KSN-CNT   (IDX-KSN)
      *
      *        褥瘡患者管理加算の算定済みの場合はコンボの表示内容変更
               IF    ( SI01-KSN-SRYCD (IDX-KSN) = "190116990" )
                AND  ( SI01-JOKUSOUKBN =  "1"   OR  "2")
                   MOVE    "算定済み"
                                   TO  SPA-GMN-I01-KSNMEIL   (IDX-KSN 1)
                                       SPA-GMN-I01-KSNMEIL   (IDX-KSN 2)
               END-IF
      *
               MOVE    SPACE           TO  WRK-CMB-CD
               PERFORM VARYING IDX-KSN2    FROM    1   BY  1
                       UNTIL ( IDX-KSN2    >   CONST-GMN-KSN-MAX )
                        OR   ( WRK-CMB-CD  NOT =   SPACE )
                   IF    ( SPA-GMN-I01-KSN-SRYCD (IDX-KSN)
                                       =   WRK-KSN-SRYCD (IDX-KSN2))
                       MOVE    WRK-KSN-CD (IDX-KSN2)
                                       TO  WRK-CMB-CD
                   END-IF
               END-PERFORM
               PERFORM 4103-CMB-KSN-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
      *
                   MOVE    "1"         TO  WRK-CMB-CD
      *
      *            褥瘡患者管理加算
                   IF      SPA-GMN-I01-KSN-SRYCD(IDX-KSN) = "190116990"
                           MOVE   "1"         TO  WRK-CMB-CD
                       IF   SPA-I01-JOKUSOUKBN   =   "1"
                           MOVE   "1"         TO  WRK-CMB-CD
                       END-IF
                       IF   SPA-I01-JOKUSOUKBN   =   "2"
                           MOVE   "2"         TO  WRK-CMB-CD
                       END-IF
                   END-IF
      *
      *            有床診療所一般病床初期加算
                   IF      SPA-GMN-I01-KSN-SRYCD(IDX-KSN) = "190135470"
                       IF    SYS-5000-IPPANKSNKBN   =   SPACE  OR  "0"
                           MOVE   "1"         TO  WRK-CMB-CD
                       ELSE
                           MOVE   "2"         TO  WRK-CMB-CD
                       END-IF
                   END-IF
      *
      *            救急・在宅等支援療養病床初期加算（病院）
      *            救急・在宅等支援療養病床初期加算（診療所）
      *            救急・在宅等支援病床初期加算（一般病棟入院基本料）
      *            救急・在宅等支援病床初期加算（特定一般病棟入院料）
      *            救急・在宅等支援病床初期加算（地域包括ケア病棟入院料）
      *            在宅患者支援療養病床初期加算（療養病棟入院基本料）
      *            在宅患者支援病床初期加算（地域包括ケア病棟入院料）
                   IF      SPA-GMN-I01-KSN-SRYCD(IDX-KSN) = "190134270"
                                                         OR "190143370"
                                                         OR "190135670"
                                                         OR "190142970"
                                                         OR "190152470"
                                                         OR "190177170"
                                                         OR "190201870"
                                                         OR "190212470"
                       IF    SYS-5000-KYUKYUKSNKBN   =   SPACE  OR  "0"
                           MOVE   "1"         TO  WRK-CMB-CD
                       ELSE
                           MOVE   "2"         TO  WRK-CMB-CD
                       END-IF
                   END-IF
      *
                   PERFORM 4103-CMB-KSN-CHK-SEC
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-KSN (IDX-KSN)
               ELSE
      *            褥瘡患者管理加算
                   IF      SPA-GMN-I01-KSN-SRYCD(IDX-KSN) = "190116990"
                           MOVE   "1"         TO  WRK-CMB-CD
                       IF   SPA-I01-JOKUSOUKBN   =   "1"
                           MOVE   "1"         TO  WRK-CMB-CD
                           MOVE   "1 算定済み"  TO  WRK-CMB-ITM
                       END-IF
                       IF   SPA-I01-JOKUSOUKBN   =   "2"
                           MOVE   "2"         TO  WRK-CMB-CD
                           MOVE   "2 算定済み"  TO  WRK-CMB-ITM
                       END-IF
                   END-IF
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-KSN (IDX-KSN)
               END-IF
      *
           END-PERFORM
      *
      *    =====   TEST  ======
      *     DISPLAY "SPA-GMN-I01-KSN-SRYCD(1) = "
      *              SPA-GMN-I01-KSN-SRYCD(1)
      *     DISPLAY "SPA-GMN-I01-KSN-SRYCD(2) = "
      *              SPA-GMN-I01-KSN-SRYCD(2)
      *     DISPLAY "SPA-GMN-I01-KSN-SRYCD(3) = "
      *              SPA-GMN-I01-KSN-SRYCD(3)
      *     MOVE   "2 算定済み"  TO  WRK-CMB-ITM
      *     MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-KSN (1)
      *     MOVE    WIDGET-INSENSITIVE
      *                          TO   I01-KSN-STT (1)
      *                               I01-KSN-ITM-STT(1)
      *    =====   TEST  ======
           .
       41011-CMB-KSN-SET-EXT.
           EXIT.
      *****************************************************************
      *    分娩入院チェック
      *****************************************************************
       41011-BUNBEN-NYUIN-CHK-SEC          SECTION.
      *
           MOVE    ZERO                    TO  FLG-BUNBENCHK
      *
           IF    ( SPA-NAI-I01-PTID        =   ZERO )
               CONTINUE
           ELSE
      *
               MOVE    SPA-NAI-I01-NYUINYMD    TO  WRK-KJNYMD
               PERFORM 41011-PT-AGE-SEC
               IF    ( SPA-NAI-I01-RECEKA  =   "23" OR "24" )
                AND  ( SPA-NAI-I01-SEX     =   "2" )
                AND  ( WRK-NENREI          >   10 )
                   MOVE    1                   TO  FLG-BUNBENCHK
               END-IF
      *
               IF    ( SPA-NAI-I01-BUNBEN  =   1 )
                   MOVE    1                   TO  FLG-BUNBENCHK
               END-IF
           END-IF
      *
           .
       41011-BUNBEN-NYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    年齢チェック
      *****************************************************************
       41011-PT-AGE-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-NENREI
      *
           MOVE    SPA-NAI-I01-BIRTHDAY
                                           TO  WRK-YMDS1
           MOVE    WRK-KJNYMD          TO  WRK-YMDS2
           COMPUTE WRK-NENREI      =   WRK-YYS2
                                   -   WRK-YYS1
           IF    ( WRK-YMDS1(5:4)     >   WRK-YMDS2(5:4) )
                COMPUTE WRK-NENREI =   WRK-NENREI  -   1
           END-IF
      *
           .
       41011-PT-AGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人判定処理
      *****************************************************************
       410111-ROUJIN-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ROUJIN
      *
           IF    ( WRK-RJNYMD         >=  CONST-H200401 )
      *
               INITIALIZE                  PTHKNINF-REC
               MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
               MOVE    SPA-NAI-I01-PTID    TO  PTHKN-PTID
               MOVE    WRK-RJNYMD          TO  PTHKN-TEKSTYMD
               MOVE    WRK-RJNYMD          TO  PTHKN-TEKEDYMD
      *
               MOVE    PTHKNINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    1               TO  FLG-ROUJIN
               END-IF
      *
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               INITIALIZE                  PTKOHINF-REC
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
               MOVE    SPA-NAI-I01-PTID    TO  PTKOH-PTID
               MOVE    "968"               TO  PTKOH-KOHNUM
               MOVE    WRK-RJNYMD          TO  PTKOH-TEKSTYMD
               MOVE    WRK-RJNYMD          TO  PTKOH-TEKEDYMD
      *
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    1               TO  FLG-ROUJIN
               END-IF
      *
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           ELSE
      *
               INITIALIZE                  PTKOHINF-REC
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
               MOVE    SPA-NAI-I01-PTID    TO  PTKOH-PTID
               MOVE    "027"               TO  PTKOH-KOHNUM
               MOVE    WRK-RJNYMD          TO  PTKOH-TEKSTYMD
               MOVE    WRK-RJNYMD          TO  PTKOH-TEKEDYMD
      *
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    1               TO  FLG-ROUJIN
               END-IF
      *
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
      *
       410111-ROUJIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボデータセット処理
      *****************************************************************
       4100-CMB-TOKNYUIN-DAT-SET-SEC   SECTION.
      *
           MOVE    WRK-TOKNYUIN-KBN    TO  WRK-CHKTOKNYUIN-KBN
           MOVE    WRK-KBNCD           TO  WRK-CHKTOKNYUIN-CD
      *
           IF    ( SPA-NAI-I01-BTUSBT  =   "04" OR "09" )
      *
                PERFORM VARYING IDX-LIFE    FROM    1   BY  1
                       UNTIL ( IDX-LIFE    >   CONST-TOK-LIFECD-MAX )
      *
                   IF     ( FLG-LIFE-RJN        =   1    OR 2 )
                       IF    ( WRK-CHKTOKNYUIN =   CONST-TOK-LIFECD-IPN
                                                             (IDX-LIFE))
                           MOVE    CONST-TOK-LIFECD-LIFE (IDX-LIFE)
                                               TO  WRK-CHKTOKNYUIN
                           MOVE    CONST-TOK-LIFECD-MAX
                                               TO  IDX-LIFE
                       END-IF
                   ELSE
                       IF    ( WRK-CHKTOKNYUIN =   CONST-TOK-LIFECD-LIFE
                                                             (IDX-LIFE))
                           MOVE    CONST-TOK-LIFECD-IPN (IDX-LIFE)
                                               TO  WRK-CHKTOKNYUIN
                           MOVE    CONST-TOK-LIFECD-MAX
                                               TO  IDX-LIFE
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           IF    ( SPA-NAI-I01-BTUSBT  =   "04" OR "09" )
            AND  ( FLG-LIFE-RJN        =   2 )
      *
               IF    ( WRK-CHKTOKNYUIN-CD  NOT =    WRK-KBNCD )
                   PERFORM 41001-CMB-TOKNYUIN-DAT-SET-SEC
               END-IF
           END-IF
      *
           MOVE    WRK-CHKTOKNYUIN-CD  TO  WRK-KBNCD
           PERFORM 41001-CMB-TOKNYUIN-DAT-SET-SEC
      *
           .
       4100-CMB-TOKNYUIN-DAT-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボデータセット処理
      *****************************************************************
       41001-CMB-TOKNYUIN-DAT-SET-SEC   SECTION.
      *
           MOVE    WRK-KJNYMD      TO  WRK-YUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               IF    ( WRK-TOKNYUIN-KBN    =   "3" )
                   MOVE    SYSKANRI-REC    TO  SYS-5111-REC
                   IF    ( SYS-5111-KBNCD  =   "23" OR "49" OR "51" )
                       MOVE    SYS-5111-SRYCD
                                           TO  WRK-SRYCD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       MOVE    TNS-NAME    TO  WRK-TOKNYUIN-NM
                   ELSE
                       MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM
                                           TO  WRK-TOKNYUIN-NM
                   END-IF
                   MOVE    SYS-5111-KBNCD
                                           TO  WRK-TOKNYUIN-CD
                   MOVE    SYS-5111-BRM-NYUINRYOKBN
                                           TO  WRK-TOKNYUIN-NYUINRYOKBN
                   MOVE    SYS-5111-SRYCD  TO  WRK-TOKNYUIN-SRYCD-X
               ELSE
                   MOVE    SYSKANRI-REC    TO  SYS-5110-REC
                   MOVE    SYS-5110-BTU-TOKUTEINYUIN-NM
                                           TO  WRK-TOKNYUIN-NM
                   MOVE    SYS-5110-KBNCD
                                           TO  WRK-TOKNYUIN-CD
                   MOVE    SPACE           TO  WRK-TOKNYUIN-NYUINRYOKBN
                   MOVE    SYS-5110-SRYCD  TO  WRK-TOKNYUIN-SRYCD-X
               END-IF
               PERFORM 4101-CMB-TOKNYUIN-DAT-SET-SEC
           END-IF
      *
           .
       41001-CMB-TOKNYUIN-DAT-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボデータセット処理
      *****************************************************************
       41011-KANSATU-DAT-SET-SEC   SECTION.
      *
           MOVE    1               TO  SPA-NAI-I01-SELNYUKHN
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5112"          TO  SYS-KANRICD
           MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY15-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5112-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    SYS-5112-KANSATUMEI TO  WRK-TOKNYUIN-NM
               MOVE    "4"                 TO  WRK-TOKNYUIN-KBN
               MOVE    SYS-5112-KBNCD      TO  WRK-TOKNYUIN-CD
      *        MOVE    SYS-5112-SRYCD      TO  WRK-TOKNYUIN-SRYCD-X
               MOVE    ZERO                TO  WRK-TOKNYUIN-SRYCD-X
               MOVE    SYS-5112-KBNCD(1:2) TO  WRK-TOKNYUIN-SRYCD-X(8:2)
               PERFORM 4101-CMB-TOKNYUIN-DAT-SET-SEC
      *
               PERFORM 900-SYSKANRI-KEY15-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5112-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       41011-KANSATU-DAT-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボデータセット処理
      *****************************************************************
       4101-CMB-TOKNYUIN-DAT-SET-SEC   SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
      *
           MOVE    ZERO                TO  FLG-PASS
           IF    ( WRK-TOKNYUIN-NM     =   SPACE )
               MOVE    1               TO  FLG-PASS
           END-IF
      *
           IF    ( WRK-TOKNYUIN-KBN    =   "4" )
               CONTINUE
           ELSE
               MOVE    WRK-TOKNYUIN-SRYCD-X
                                       TO  WRK-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-HOSPSRYKBN  TO  WRK-HOSPSBT
               EVALUATE    TNS-HOSPSRYKBN
               WHEN    3
                   MOVE    0           TO  WRK-HOSPSBT
               WHEN    4
                   MOVE    1           TO  WRK-HOSPSBT
               END-EVALUATE
               IF    ( WRK-HOSPSBT     =   0 )
                OR   ( WRK-HOSPSBT     =   SPA-HOSPSBT )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-PASS
               END-IF
           END-IF
      *
           PERFORM VARYING IDX17   FROM    1   BY  1
                   UNTIL ( IDX17   >   CONST-TOKNYUIN-MAX )
                    OR   ( FLG-PASS    =   1 )
      *
               IF    ( SPA-GMN-I01-TOKNYUIN-ITM2 (IDX17)
                                       =   WRK-TOKNYUIN )
                   MOVE    1           TO  FLG-PASS
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-I01-TOKNYUIN-CNT   >=   CONST-TOKNYUIN-MAX )
            OR   ( FLG-PASS                    =   1 )
               CONTINUE
           ELSE
               COMPUTE SPA-GMN-I01-TOKNYUIN-CNT
                   =   SPA-GMN-I01-TOKNYUIN-CNT    +   1
               MOVE    SPA-GMN-I01-TOKNYUIN-CNT
                                       TO  IDX10
               MOVE    IDX10           TO  WRK-99
               MOVE    WRK-99          TO  SPA-GMN-I01-TOKNYUINL
                                                       (IDX10)
               MOVE    WRK-TOKNYUIN-NM TO  SPA-GMN-I01-TOKNYUINMEIL
                                                       (IDX10)
               MOVE    WRK-TOKNYUIN-KBN
                                       TO  SPA-GMN-I01-TOKNYUIN-ITM2
                                                       (IDX10) (1 : 1)
               MOVE    WRK-TOKNYUIN-CD
                                       TO  SPA-GMN-I01-TOKNYUIN-ITM2
                                                       (IDX10) (2 : 2)
               MOVE    WRK-TOKNYUIN-NYUINRYOKBN
                                       TO  SPA-GMN-I01-TOKNYUIN-ITM3
                                                       (IDX10)
               MOVE    WRK-TOKNYUIN-SRYCD-X
                                       TO  SPA-GMN-I01-TOKNYUIN-ITM4
                                                       (IDX10)
      *
               IF    ( WRK-TOKNYUIN-KBN    =    "2" OR "3" )
                   PERFORM 800-TOKHIGAITO-CHK-SEC
                   MOVE    WRK-TOKHIGAITO-CD
                                       TO  SPA-GMN-I01-TOKNYUIN-ITM5
                                                       (IDX10)
               END-IF
      *
               IF    ( WRK-TOKNYUIN-SRYCD-X    IS  NUMERIC )
                   COMPUTE SPA-GMN-I01-TOKNYUIN-SRYCDTTL
                       =   SPA-GMN-I01-TOKNYUIN-SRYCDTTL
                       +   WRK-TOKNYUIN-SRYCD
               END-IF
      *
               IF    ( TNS-CDKBN-KBN       =   "A" )
                 AND ( TNS-CDKBN-KBNNUM   >=   100 )
                 AND ( TNS-CDKBN-KBNNUM   <=   199 )
                   MOVE    1           TO  SPA-NAI-I01-SELNYUKHN
               END-IF
      *
           END-IF
      *
           .
       4101-CMB-TOKNYUIN-DAT-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料初期値設定処理
      *****************************************************************
       4101-TOKNYUIN-SYOKI-SEC         SECTION.
      *
           INITIALIZE  SPA-GMN-I01-TOKNYUIN
                       SPA-GMN-I01-SNTNYUIN
                       SPA-NAI-I01-SNTNYUINCD
                       SPA-NAI-I01-TOKNYUIN-ORG
                       SPA-NAI-I01-TOKNYUINCD
                       SPA-NAI-I01-TOKHIGAITOCD
      *
           INITIALIZE  SPA-GMN-I01-TOKHIGAITO
      *
           PERFORM VARYING IDX10
                   FROM    SPA-GMN-I01-TOKNYUIN-CNT    BY  -1
                   UNTIL ( IDX10   <   1 )
                    OR   ( SPA-GMN-I01-TOKNYUIN NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-TOKNYUIN-ITM2 (IDX10)
                                   =   SPA-NAI-I01-BTUTOKCD
                                    OR SPA-NAI-I01-BRMTOKCD )
      *
                   MOVE    SPA-GMN-I01-TOKNYUIN-ITM  (IDX10)
                                       TO  SPA-GMN-I01-TOKNYUIN
                   MOVE    SPA-GMN-I01-TOKNYUIN-ITM2 (IDX10)
                                       TO  SPA-NAI-I01-TOKNYUIN-ORG
                   MOVE    SPA-GMN-I01-TOKNYUIN-ITM4 (IDX10)
                                       TO  SPA-NAI-I01-TOKNYUINCD
                   MOVE    SPA-GMN-I01-TOKNYUIN-ITM5 (IDX10)
                                       TO  SPA-NAI-I01-TOKHIGAITOCD
      *
               END-IF
           END-PERFORM
      *
           IF    ( SPA-GMN-I01-TOKNYUIN            =   SPACE )
               IF    ( SPA-NAI-I01-BRMKHNKBNCD NOT =   SPACE )
                   MOVE    SPA-NAI-I01-BRMKHNKBNCD
                                           TO  WRK-CMB-CD
                   IF    ( WRK-KJNYMD      >=  CONST-H181001 )
                       PERFORM 41011-TOKNYUIN-SYOKI-SEC
                   END-IF
                   PERFORM 4103-CMB-TOKNYUIN-SRH2-SEC
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TOKNYUIN
                   MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I01-TOKNYUIN-ORG
                   MOVE    WRK-CMB-ITM4    TO  SPA-NAI-I01-TOKNYUINCD
                   MOVE    WRK-CMB-ITM5    TO  SPA-NAI-I01-TOKHIGAITOCD
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-TOKNYUIN            =   SPACE )
               IF    ( SPA-NAI-I01-BTUKHNKBNCD NOT =   SPACE )
                   MOVE    SPA-NAI-I01-BTUKHNKBNCD
                                       TO  WRK-CMB-CD
                   IF    ( WRK-KJNYMD      >=  CONST-H181001 )
                       PERFORM 41011-TOKNYUIN-SYOKI-SEC
                   END-IF
      *             DISPLAY "WRK-CMB-CD ===" WRK-CMB-CD
                   PERFORM 4103-CMB-TOKNYUIN-SRH2-SEC
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TOKNYUIN
                   MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I01-TOKNYUIN-ORG
                   MOVE    WRK-CMB-ITM4    TO  SPA-NAI-I01-TOKNYUINCD
                   MOVE    WRK-CMB-ITM5    TO  SPA-NAI-I01-TOKHIGAITOCD
               END-IF
           END-IF
      *
      *    算定入院料についても初期値を設定
           IF    ( SPA-GMN-I01-TOKNYUIN    =   SPACE )
               MOVE    SPA-NAI-I01-KHNRYO-NM
                                       TO  SPA-GMN-I01-SNTNYUIN
               MOVE    SPA-NAI-I01-KHNRYO-CD
                                       TO  SPA-NAI-I01-SNTNYUINCD
           ELSE
      *
               MOVE    SPA-GMN-I01-TOKNYUIN-NM
                                       TO  SPA-GMN-I01-SNTNYUIN
               MOVE    SPA-NAI-I01-TOKNYUINCD
                                       TO  SPA-NAI-I01-SNTNYUINCD
           END-IF
      *
           MOVE    "0"                 TO  SPA-GMN-I01-TOKHIGAITO-CD
           PERFORM 4101-TOKHIGAITO-CHK-SEC
      *
           .
       4101-TOKNYUIN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料初期値設定処理
      *****************************************************************
       41011-TOKNYUIN-SYOKI-SEC        SECTION.
      *
           IF    ( SPA-NAI-I01-BTUSBT  =   "04" OR "09" )
               MOVE    WRK-KJNYMD  TO  WRK-AGECHK-SRYYMD
               PERFORM 4801-RJN-TAISYO-CHK-SEC
               IF    ( FLG-LIFE-RJN                =   1  OR 2)
                   PERFORM VARYING IDX10   FROM    1   BY  1
                           UNTIL ( IDX10 > CONST-TOK-LIFECD-MAX)
                       IF    ( WRK-CMB-CD
                               =   CONST-TOK-LIFECD-IPN (IDX10))
                           MOVE    CONST-TOK-LIFECD-LIFE (IDX10)
                                       TO  WRK-CMB-CD
                           MOVE    CONST-TOK-LIFECD-MAX
                                       TO  IDX10
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       41011-TOKNYUIN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    病棟名称チェック処理
      *****************************************************************
       4101-BTUNAME-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-I01-BTUNAME =   SPACE )
               GO  TO  4101-BTUNAME-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-BTU-KA-CD
                                           WRK-BRM-KA-CD
      *
           MOVE    SPA-GMN-I01-BTUNAME-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-BTUNAME-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-BTUNAME
               MOVE    WRK-CMB-ITM3 TO WRK-BTU-KA-CD
               MOVE    WRK-CMB-ITM4 TO WRK-BRM-KA-CD
      *        室料差額のデフォルト値を設定
               MOVE    WRK-CMB-ITM2
                                   TO  WRK-CMB-CD
               PERFORM 4103-CMB-SAGAKU-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SAGAKU
      *
      *        診療科の初期値を設定
               IF    ( WRK-BRM-KA-CD   NOT =   SPACE )
                   MOVE    WRK-BRM-KA-CD       TO  WRK-CMB-CD
                   PERFORM 4103-CMB-NYUINKA-CHK-SEC
                   MOVE    WRK-CMB-ITM         TO  SPA-GMN-I01-NYUINKA
                   MOVE    WRK-CMB-ITM2        TO  SPA-NAI-I01-RECEKA
               ELSE
                       MOVE    WRK-BTU-KA-CD   TO  WRK-CMB-CD
                       PERFORM 4103-CMB-NYUINKA-CHK-SEC
                       MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-NYUINKA
                       MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I01-RECEKA
               END-IF
           ELSE
               MOVE    "0015"      TO  SPA-ERRCD
               MOVE    5           TO  SPA-GMN-I01-CUR
               GO  TO  4101-BTUNAME-CHK-EXT
           END-IF
      *
      *    特定入院料コンボ設定処理
           MOVE    WRK-KIHON-KJNYMD    TO  WRK-KJNYMD
           PERFORM 4101-CMB-TOKNYUIN-SET-SEC
      *
      *    特定入院料の初期値を設定
           MOVE    WRK-KIHON-KJNYMD    TO  WRK-KJNYMD
           PERFORM 4101-TOKNYUIN-SYOKI-SEC
      *
           .
      *
       4101-BTUNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額チェック処理
      *****************************************************************
       4101-SAGAKU-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I01-SAGAKU  =   SPACE )
               GO  TO  4101-SAGAKU-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-SAGAKU-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-SAGAKU-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SAGAKU
           ELSE
               MOVE    "0016"      TO  SPA-ERRCD
               MOVE    6           TO  SPA-GMN-I01-CUR
               GO  TO  4101-SAGAKU-CHK-EXT
           END-IF
      *
           .
      *
       4101-SAGAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事区分チェック処理
      *****************************************************************
       4101-SKJKBN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-I01-SKJKBN-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-SKJKBN-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SKJKBN
           ELSE
               IF    ( SPA-GMN-I01-SYORIKBN-CD  =   "08" )
                   IF    ( SPA-NAI-I01-MAESKJKBN    =  "B" OR "C" )
                       MOVE    "B"      TO  WRK-CMB-CD
                   ELSE
                       MOVE    "A"      TO  WRK-CMB-CD
                   END-IF
               ELSE
                   MOVE    "1"          TO  WRK-CMB-CD
               END-IF
               PERFORM 4103-CMB-SKJKBN-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SKJKBN
           END-IF
      *
           IF  SPA-GMN-I01-SYORIKBN-CD  =   "01"
             IF    ( SPA-GMN-I01-SKJKBN-CD   =   "0" )
                 MOVE    "0"         TO  SPA-GMN-I01-TIMEKBN-CD
             END-IF
           END-IF
      *
           IF ( SPA-GMN-I01-SYORIKBN-CD  =   "01"  ) AND
              ( SPA-GMN-I01-SKJKBN-CD    =   "A" OR "B" )
               IF    ( SPA-NAI-I01-MAEIDOYMD    =   SPACE )
                   MOVE    "1049"      TO  SPA-ERRCD
                   MOVE    21          TO  SPA-GMN-I01-CUR
                   GO  TO  4101-SKJKBN-CHK-EXT
               END-IF
           END-IF
      *
           IF    ( WRK-SKJKBN-MAE      NOT =   SPACE )
               EVALUATE     TRUE
               WHEN  ( SPA-GMN-I01-SKJKBN-CD   NOT =   "B" )
                AND  ( SPA-GMN-I01-SKJKBN-CD   NOT =   "C" )
                   MOVE    SPACE       TO  SPA-GMN-I01-TSKJKBN-ASA
                                           SPA-GMN-I01-TSKJKBN-HIRU
                                           SPA-GMN-I01-TSKJKBN-YU
               WHEN  ( SPA-GMN-I01-SKJKBN-CD        =   "B" )
                AND  ( WRK-SKJKBN-MAE           NOT =   "B" )
               WHEN  ( SPA-GMN-I01-SKJKBN-CD        =   "C" )
                AND  ( WRK-SKJKBN-MAE           NOT =   "C" )
                   MOVE    SPA-NAI-I01-SKJKBN-DEF
                                       TO  WRK-CMB-CD
                   PERFORM 4103-CMB-TSKJKBN-CHK-SEC
                   IF    ( WRK-CMB-ITM =   SPACE )
                       MOVE    "1"     TO  WRK-CMB-CD
                       PERFORM 4103-CMB-TSKJKBN-CHK-SEC
                   END-IF
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-TSKJKBN-ASA
                                           SPA-GMN-I01-TSKJKBN-HIRU
                                           SPA-GMN-I01-TSKJKBN-YU
                   IF   (( SPA-GMN-I01-SYORIKBN-CD      =   "01" )
                      AND( SPA-NAI-I01-MAEIDOYMD    NOT =   SPACE ))
                      OR ( SPA-GMN-I01-SYORIKBN-CD      =   "08" )
                     IF  ( SPA-NAI-I01-MAESKJKBN    =   "B" OR "C" )
                       PERFORM VARYING IDXA    FROM    1   BY  1
                               UNTIL ( IDXA    >   3 )
                          MOVE    SPA-NAI-I01-TMAESKJKBN (IDXA)
                                       TO   WRK-CMB-CD
                          PERFORM 4103-CMB-TSKJKBN-CHK-SEC
                          MOVE    WRK-CMB-ITM
                                       TO   SPA-GMN-I01-TSKJKBN (IDXA)
                       END-PERFORM
                     END-IF
                   END-IF
               END-EVALUATE
           END-IF
      *
           .
      *
       4101-SKJKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事区分（個別指示）チェック処理
      *****************************************************************
       4101-TSKJKBN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-I01-TSKJKBN-CD (IDX1)
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-TSKJKBN-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-TSKJKBN (IDX1)
           ELSE
               MOVE    SPA-NAI-I01-SKJKBN-DEF
                                   TO  WRK-CMB-CD
               PERFORM 4103-CMB-TSKJKBN-CHK-SEC
               IF    ( WRK-CMB-ITM =   SPACE )
                   MOVE    "1"     TO  WRK-CMB-CD
                   PERFORM 4103-CMB-TSKJKBN-CHK-SEC
               END-IF
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-TSKJKBN (IDX1)
           END-IF
      *
           .
      *
       4101-TSKJKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事時間区分チェック処理
      *****************************************************************
       4101-TIMEKBN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-I01-TIMEKBN-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-TIMEKBN-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-I01-TIMEKBN
      *
           .
      *
       4101-TIMEKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定入院区分チェック処理
      *****************************************************************
       4101-SENTEIKBN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-I01-SENTEIKBN-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-SENTEIKBN-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SENTEIKBN
           ELSE
               MOVE    "1"             TO  WRK-CMB-CD
               PERFORM 4103-CMB-SENTEIKBN-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SENTEIKBN
           END-IF
      *
           .
      *
       4101-SENTEIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院科チェック処理
      *****************************************************************
       4101-NYUINKA-CHK-SEC            SECTION.
      *
      *
           IF    ( SPA-GMN-I01-NYUINKA =   SPACE )
               GO  TO  4101-NYUINKA-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-NYUINKA-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-NYUINKA-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-NYUINKA
               MOVE    WRK-CMB-ITM2 TO SPA-NAI-I01-RECEKA
           ELSE
               MOVE    "0018"      TO  SPA-ERRCD
               MOVE    8           TO  SPA-GMN-I01-CUR
               GO  TO  4101-NYUINKA-CHK-EXT
           END-IF
      *
           .
      *
       4101-NYUINKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初回チェック処理
      *****************************************************************
       4101-SHOKAI-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I01-SHOKAI  =   SPACE )
               GO  TO  4101-SHOKAI-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-SHOKAI-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-SHOKAI-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SHOKAI
           ELSE
               MOVE    "0019"      TO  SPA-ERRCD
               MOVE    9           TO  SPA-GMN-I01-CUR
               GO  TO  4101-SHOKAI-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "01" )
               IF    ( SPA-GMN-I01-SHOKAI-CD   =   "2")
                   CONTINUE
               ELSE
                   MOVE    ZERO            TO  SPA-GMN-I01-SHONUM
                   MOVE    SPACE       TO  SPA-GMN-I01-LBL-SHONYUINYMD
               END-IF
           END-IF
      *
           .
      *
       4101-SHOKAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初回番号チェック処理
      *****************************************************************
       4101-SHONUM-CHK-SEC             SECTION.
      *
           MOVE    SPACE       TO      WRK-SHOKAI-NYUINYMD-W
           MOVE    ZERO        TO      FLG-SHONUM
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD NOT =   "01" )
            OR   ( SPA-GMN-I01-SHONUM          =   ZERO )
               GO  TO  4101-SHONUM-CHK-EXT
           END-IF
      *
           MOVE    SPACE       TO      SPA-GMN-I01-LBL-SHONYUINYMD
      *
      D    DISPLAY "SPA-GMN-I01-SHONUM  =" SPA-GMN-I01-SHONUM
      D    DISPLAY "SPA-NAI-I01-PTRRKNUM=" SPA-NAI-I01-PTRRKNUM
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I01-NYUREKILST-CNT )
                    OR   ( FLG-SHONUM      =   1 )
      *
               IF    ( SPA-GMN-I01-TSHONUM (IDX1)
                                   =   SPA-GMN-I01-SHONUM )
                 AND ( SPA-NAI-I01-TRRKNUM (IDX1)
                               NOT =   SPA-NAI-I01-PTRRKNUM )
                 AND ( SPA-GMN-I01-TSHOKAI (IDX1)  =   "1" )
      *
                   MOVE      SPA-GMN-I01-TNYUINYMD (IDX1)
                                   TO  WRK-SHOKAI-NYUINYMD-W
                   MOVE      1     TO  FLG-SHONUM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-SHONUM      =   ZERO )
               INITIALIZE                  WKPTNYUIN-REC
               MOVE    SPA-HOSPNUM      TO  WKPTNYUIN-HOSPNUM
               MOVE    SPA-NAI-I01-PTID
                                       TO  WKPTNYUIN-PTID
               MOVE    SPA-GMN-I01-SHONUM
                                       TO  WKPTNYUIN-SHONUM
               MOVE    WKPTNYUIN-REC
                                       TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  WKPTNYUIN-REC
                   IF    ( WKPTNYUIN-RRKNUM
                               NOT =   SPA-NAI-I01-PTRRKNUM )
                       MOVE    WKPTNYUIN-NYUINYMD
                                       TO  WRK-YMD
                       PERFORM 5002-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG
                                       TO  WRK-SHOKAI-NYUINYMD-W
      *
                       MOVE      1     TO  FLG-SHONUM
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
           IF    ( FLG-SHONUM      =   1 )
               STRING "入院日 "
                      WRK-SHOKAI-NYUINYMD-W
               DELIMITED   BY  SIZE
               INTO   SPA-GMN-I01-LBL-SHONYUINYMD
               END-STRING
           ELSE
               MOVE    10          TO      SPA-GMN-I01-CUR
               MOVE    "0005"      TO      SPA-ERRCD
               GO  TO  4101-SHONUM-CHK-EXT
           END-IF
      *
           .
       4101-SHONUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料非該当チェック処理
      *****************************************************************
       4101-TOKHIGAITO-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-I01-TOKHIGAITO-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-TOKHIGAITO-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-TOKHIGAITO
           ELSE
               MOVE    "0"         TO  WRK-CMB-CD
               PERFORM 4103-CMB-TOKHIGAITO-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-TOKHIGAITO
           END-IF
      *
           .
      *
       4101-TOKHIGAITO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料算定区分チェック処理
      *****************************************************************
       4101-NYUCALKBNCHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-I01-NYUCALKBN-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-NYUCALKBN-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-NYUCALKBN
           ELSE
               MOVE    "2"         TO  WRK-CMB-CD
               PERFORM 4103-CMB-NYUCALKBN-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-NYUCALKBN
           END-IF
      *
           .
      *
       4101-NYUCALKBNCHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分チェック処理
      *****************************************************************
       4101-BUNBEN-CHK-SEC             SECTION.
      *
           PERFORM 41011-BUNBEN-NYUIN-CHK-SEC
           IF    ( FLG-BUNBENCHK       =   1 )
               MOVE    SPA-GMN-I01-BUNBEN-CD
                                       TO  WRK-CMB-CD
               PERFORM 4103-CMB-BUNBEN-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-BUNBEN
               IF    ( SPA-GMN-I01-BUNBEN  =   SPACE )
                 AND ( SPA-GMN-I01-SYORIKBN-CD
                                       =   "01" OR "03" OR "08" )
                   MOVE    "1"         TO  WRK-CMB-CD
                   PERFORM 4103-CMB-BUNBEN-CHK-SEC
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-BUNBEN
               END-IF
           ELSE
                   MOVE    SPACE       TO  SPA-GMN-I01-BUNBEN
           END-IF
      *
           .
      *
       4101-BUNBEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分チェック処理
      *****************************************************************
       4101-RIYOU-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I01-BUNBEN-CD   =   "1" OR "2" )
      *
               MOVE    SPA-GMN-I01-RIYOU-CD
                                           TO  WRK-CMB-CD
               PERFORM 4103-CMB-RIYOU-CHK-SEC
               IF    ( WRK-CMB-ITM     NOT =   SPACE )
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-RIYOU
               ELSE
                   MOVE    SPA-GMN-I01-RIYOU-ITM (2)
                                       TO  SPA-GMN-I01-RIYOU
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-GMN-I01-RIYOU
           END-IF
      *
           .
      *
       4101-RIYOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護異動チェック処理
      *****************************************************************
       4101-KAIGO-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I01-KAIGO  =   SPACE )
               GO  TO  4101-KAIGO-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-KAIGO-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-KAIGO-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-KAIGO
           ELSE
               MOVE    "0029"      TO  SPA-ERRCD
               MOVE    26          TO  SPA-GMN-I01-CUR
               GO  TO  4101-KAIGO-CHK-EXT
           END-IF
      *
           .
      *
       4101-KAIGO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    担当医チェック処理
      *****************************************************************
       4101-DR-CHK-SEC                 SECTION.
      *
           IF    ( SPA-GMN-I01-DR (IDX1)  =   SPACE )
               GO  TO  4101-DR-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-DR-CD (IDX1)
                                   TO  WRK-CMB-CD
           MOVE    WRK-KIHON-KJNYMD TO WRK-KJNYMD
           PERFORM 4103-CMB-DR-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-DR (IDX1)
           ELSE
               MOVE    "0022"      TO  SPA-ERRCD
               COMPUTE SPA-GMN-I01-CUR =   IDX1    +   11
               GO  TO  4101-DR-CHK-EXT
           END-IF
      *
           .
      *
       4101-DR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合わせチェック処理
      *****************************************************************
       4101-HKNCOMBI-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD    =   "01" OR  "08")
               CONTINUE
           ELSE
               GO  TO  4101-HKNCOMBI-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-I01-HKNCOMBI-CNT   >   ZERO )
               CONTINUE
           ELSE
               MOVE    SPACE       TO  SPA-GMN-I01-HKNCOMBI
               MOVE    "1021"      TO  SPA-ERRCD
               MOVE    15          TO  SPA-GMN-I01-CUR
               GO  TO  4101-HKNCOMBI-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-I01-HKNCOMBI
                                       =   SPACE )
               GO  TO  4101-HKNCOMBI-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-HKNCOMBI-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-HKNCOMBI-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-HKNCOMBI
               MOVE    WRK-CMB-ITM2
                                   TO  SPA-NAI-I01-FTNRATE
               MOVE    WRK-CMB-ITM3
                                   TO  SPA-NAI-I01-RSI-HKNKBN
           ELSE
               MOVE    "0023"      TO  SPA-ERRCD
               MOVE    15          TO  SPA-GMN-I01-CUR
               GO  TO  4101-HKNCOMBI-CHK-EXT
           END-IF
      *
           IF    ( WRK-MCP-WIDGET  =   "HKNCOMBI" )
               MOVE    SPA-GMN-I01-TOKNYUIN-SRYCDTTL
                                   TO  WRK-TOKNYUIN-SRYCDTTL
               MOVE    WRK-KIHON-KJNYMD    TO  WRK-KJNYMD
               PERFORM 4101-CMB-TOKNYUIN-SET-SEC
               IF    ( SPA-GMN-I01-TOKNYUIN-SRYCDTTL
                               NOT =   WRK-TOKNYUIN-SRYCDTTL  )
                   MOVE    WRK-KIHON-KJNYMD    TO  WRK-KJNYMD
                   PERFORM 4101-TOKNYUIN-SYOKI-SEC
               END-IF
      *
           END-IF
      *
           .
      *
       4101-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院チェック処理（算定入院料の設定も行う）
      *****************************************************************
       4101-TOKNYUIN-CHK-SEC           SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-I01-TOKNYUIN-ORG
                                       SPA-GMN-I01-SNTNYUIN
                                       SPA-NAI-I01-SNTNYUINCD
                                       SPA-NAI-I01-TOKNYUINCD
                                       SPA-NAI-I01-TOKHIGAITOCD
      *
           IF    ( SPA-GMN-I01-TOKNYUIN
                                       =   SPACE )
               MOVE    SPA-NAI-I01-KHNRYO-NM
                                   TO  SPA-GMN-I01-SNTNYUIN
               MOVE    SPA-NAI-I01-KHNRYO-CD
                                   TO  SPA-NAI-I01-SNTNYUINCD
      *
               MOVE    WRK-KIHON-KJNYMD    TO  WRK-KJNYMD
               PERFORM 4101-CMB-KSN-SET-SEC
      *
               MOVE    "0"         TO  SPA-GMN-I01-TOKHIGAITO-CD
               PERFORM 4101-TOKHIGAITO-CHK-SEC
           ELSE
      *
               MOVE    SPA-GMN-I01-TOKNYUIN-CD
                                   TO  WRK-CMB-CD
               PERFORM 4103-CMB-TOKNYUIN-CHK-SEC
               IF    ( WRK-CMB-ITM     NOT =   SPACE )
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-TOKNYUIN
                   MOVE    WRK-CMB-ITM2
                                       TO  SPA-NAI-I01-TOKNYUIN-ORG
                   MOVE    WRK-CMB-ITM4
                                       TO  SPA-NAI-I01-TOKNYUINCD
                   MOVE    WRK-CMB-ITM5
                                       TO  SPA-NAI-I01-TOKHIGAITOCD
      *
                   PERFORM 4101-TOKHIGAITO-CHK-SEC
                   IF    ( SPA-NAI-I01-TOKHIGAITOCD    NOT =   SPACE )
                    AND  ( SPA-GMN-I01-TOKHIGAITO-CD       =   "1" )
                       MOVE    WRK-KIHON-KJNYMD
                                       TO  WRK-KJNYMD
                       MOVE    SPA-NAI-I01-TOKHIGAITOCD
                                       TO  WRK-SRYCD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       MOVE    TNS-NAME    TO  SPA-GMN-I01-SNTNYUIN
                       MOVE    TNS-SRYCD   TO  SPA-NAI-I01-SNTNYUINCD
                   ELSE
                       MOVE    SPA-NAI-I01-TOKNYUINCD
                                       TO  SPA-NAI-I01-SNTNYUINCD
                       MOVE    SPA-GMN-I01-TOKNYUIN-NM
                                       TO  SPA-GMN-I01-SNTNYUIN
                   END-IF
               ELSE
                   MOVE    "0024"      TO  SPA-ERRCD
                   MOVE    16          TO  SPA-GMN-I01-CUR
               END-IF
      *
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    WRK-KIHON-KJNYMD    TO  WRK-KJNYMD
               PERFORM 4101-CMB-KSN-SET-SEC
           END-IF
      *
           .
      *
       4101-TOKNYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    加算チェック処理
      *****************************************************************
       4101-KSN-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-I01-KSN-CD (IDX-KSN)
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-KSN-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-KSN (IDX-KSN)
           ELSE
               MOVE    "0030"      TO  SPA-ERRCD
               COMPUTE SPA-GMN-I01-CUR
                   =   IDX-KSN     +   26
           END-IF
      *
           .
      *
       4101-KSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    前受け金チェック処理
      *****************************************************************
       4101-MAEUKE-CHK-SEC             SECTION.
      *
      *
           .
      *
       4101-MAEUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求チェック処理
      *****************************************************************
       4101-SEIKYU-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I01-SEIKYU
                                       =   SPACE )
               GO  TO  4101-SEIKYU-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-SEIKYU-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SEIKYU-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SEIKYU
           ELSE
               MOVE    "0025"      TO  SPA-ERRCD
               MOVE    18          TO  SPA-GMN-I01-CUR
               GO  TO  4101-SEIKYU-CHK-EXT
           END-IF
      *
           .
      *
       4101-SEIKYU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索時患者表示チェック処理
      *****************************************************************
       4101-DISPKBN-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-I01-DISPKBN
                                       =   SPACE )
               GO  TO  4101-DISPKBN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-DISPKBN-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-DISPKBN-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-DISPKBN
           ELSE
               MOVE    "0026"      TO  SPA-ERRCD
               MOVE    19          TO  SPA-GMN-I01-CUR
               GO  TO  4101-DISPKBN-CHK-EXT
           END-IF
      *
           .
      *
       4101-DISPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザープログラムチェック処理
      *****************************************************************
       4101-USERPG-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-I01-USERPG-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-USERPG-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               IF    ( SPA-NAI-I01-GYOUMU-ARI  =   "1" )
                   IF    ( SPA-NAI-I01-GYOUMU-FLG  =   "1" )
                       MOVE    SPA-GMN-I01-USERPG-ITM (2)
                                   TO  SPA-GMN-I01-USERPG
                   ELSE
                       MOVE    SPA-GMN-I01-USERPG-ITM (1)
                                   TO  SPA-GMN-I01-USERPG
                   END-IF
               ELSE
                   MOVE    SPA-GMN-I01-USERPG-ITM (1)
                                   TO  SPA-GMN-I01-USERPG
               END-IF
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-USERPG
           END-IF
      *
           .
      *
       4101-USERPG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I01-SELNUM  =   ZERO )
               GO  TO  4101-SELNUM-CHK-EXT
           END-IF
      *
      *    入力された番号が存在するかチェック
           IF    ( SPA-GMN-I01-SELNUM  >   SPA-GMN-I01-NYUREKILST-CNT )
               MOVE    "0027"      TO  SPA-ERRCD
               MOVE    100         TO  SPA-GMN-I01-CUR
               GO  TO  4101-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-SELNUM
                                   TO  SPA-GMN-I01-NYUREKILST-SEL
      *
           .
      *
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院歴リスト選択処理
      *****************************************************************
       4101-NYUREKILST-SEL-SEC         SECTION.
      *
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >   SPA-GMN-I01-NYUREKILST-CNT )
      *
               IF    ( I01-NYUREKILST-SELECT (IDX1)    =   "T"  )
                   MOVE    IDX1        TO  SPA-GMN-I01-NYUREKILST-SEL
                   MOVE    SPA-GMN-I01-TSELNUM (IDX1)
                                       TO  I01-SELNUM
                   MOVE    SPA-GMN-I01-NYUREKILST-CNT
                                       TO  IDX1
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4101-NYUREKILST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボチェック処理
      *****************************************************************
       4103-CMB-SYORIKBN-CHK-SEC   SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-SYORIKBN-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-SYORIKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-SYORIKBN-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-SYORIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟名コンボチェック処理
      *****************************************************************
       4103-CMB-BTUNAME-CHK-SEC    SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-BTUNAME-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-BTUNAMEL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-BTUNAME-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
                   MOVE    SPA-GMN-I01-BTUNAME-ITM2 (IDX-CMB)
                                   TO  WRK-CMB-ITM2
                   MOVE    SPA-GMN-I01-BTUNAME-ITM3 (IDX-CMB)
                                   TO  WRK-CMB-ITM3
                   MOVE    SPA-GMN-I01-BTUNAME-ITM4 (IDX-CMB)
                                   TO  WRK-CMB-ITM4
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-BTUNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院事由コンボチェック処理
      *****************************************************************
       4103-CMB-TAIINCD-CHK-SEC    SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-TAIINCD-CNT )
                    OR   ( WRK-CMB-ITM     NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-TAIINCDL (IDX-CMB)
                                               =   WRK-CMB-CD )
                   MOVE    SPA-GMN-I01-TAIINCD-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4103-CMB-TAIINCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額コンボチェック処理
      *****************************************************************
       4103-CMB-SAGAKU-CHK-SEC     SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-SAGAKU-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-SAGAKUL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-SAGAKU-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-SAGAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事区分コンボチェック処理
      *****************************************************************
       4103-CMB-SKJKBN-CHK-SEC     SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-SKJKBN-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-SKJKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-SKJKBN-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-SKJKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事区分（朝昼夕）コンボチェック処理
      *****************************************************************
       4103-CMB-TSKJKBN-CHK-SEC     SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-TSKJKBN-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-TSKJKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-TSKJKBN-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-TSKJKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事時間区分コンボチェック処理
      *****************************************************************
       4103-CMB-TIMEKBN-CHK-SEC     SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-TIMEKBN-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-TIMEKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-TIMEKBN-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               IF    ( SPA-GMN-I01-TIMEKBN-CNT >=  2 )
                   MOVE    SPA-GMN-I01-TIMEKBN-ITM (2)
                               TO  WRK-CMB-ITM
               ELSE
                   MOVE    SPA-GMN-I01-TIMEKBN-ITM (1)
                               TO  WRK-CMB-ITM
               END-IF
           END-IF
      *
           .
      *
       4103-CMB-TIMEKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定入院区分コンボチェック処理
      *****************************************************************
       4103-CMB-SENTEIKBN-CHK-SEC     SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-SENTEIKBN-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-SENTEIKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-SENTEIKBN-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-SENTEIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院科コンボチェック処理
      *****************************************************************
       4103-CMB-NYUINKA-CHK-SEC    SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-NYUINKA-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-NYUINKAL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-NYUINKA-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
                   MOVE    SPA-GMN-I01-NYUINKA-ITM2(IDX-CMB)
                                   TO  WRK-CMB-ITM2
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-NYUINKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初回コンボチェック処理
      *****************************************************************
       4103-CMB-SHOKAI-CHK-SEC     SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-SHOKAI-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-SHOKAIL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-SHOKAI-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-SHOKAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護異動コンボチェック処理
      *****************************************************************
       4103-CMB-KAIGO-CHK-SEC     SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-KAIGO-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-KAIGOL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-KAIGO-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-KAIGO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料算定コンボチェック処理
      *****************************************************************
       4103-CMB-NYUCALKBN-CHK-SEC     SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-NYUCALKBN-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-NYUCALKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-NYUCALKBN-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-NYUCALKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩入院コンボチェック処理
      *****************************************************************
       4103-CMB-BUNBEN-CHK-SEC     SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-BUNBEN-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-BUNBENL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-BUNBEN-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-BUNBEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分コンボチェック処理
      *****************************************************************
       4103-CMB-RIYOU-CHK-SEC     SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-RIYOU-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-RIYOUL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-RIYOU-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-RIYOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    担当医コンボチェック処理
      *****************************************************************
       4103-CMB-DR-CHK-SEC         SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-DR-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-DRL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-DR-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
      *
               MOVE    "1010"          TO  WRK-KANRICD
               MOVE    "1"             TO  WRK-KBNCD (1:1)
               MOVE    WRK-CMB-CD      TO  WRK-KBNCD (2:)
               MOVE    WRK-KJNYMD      TO  WRK-YUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    SYSKANRI-REC    TO  SYS-1010-REC
                   MOVE    SYS-1010-KBNCD (2:4)
                                           TO  WRK-CMB-ITM (1:4)
                   MOVE    SYS-1010-NAME   TO  WRK-CMB-ITM (6:)
               END-IF
      *
           END-IF
      *
           .
      *
       4103-CMB-DR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組み合わせコンボチェック処理
      *****************************************************************
       4103-CMB-HKNCOMBI-CHK-SEC   SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-HKNCOMBI-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-HKNCOMBIL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-HKNCOMBI-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
                   MOVE    SPA-GMN-I01-HKNCOMBI-ITM2 (IDX-CMB)
                                   TO  WRK-CMB-ITM2
                   MOVE    SPA-GMN-I01-HKNCOMBI-ITM3 (IDX-CMB)
                                   TO  WRK-CMB-ITM3
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院コンボチェック処理
      *    コンボに表示されている番号でチェック
      *****************************************************************
       4103-CMB-TOKNYUIN-CHK-SEC   SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-TOKNYUIN-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-TOKNYUINL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-TOKNYUIN-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
                   MOVE    SPA-GMN-I01-TOKNYUIN-ITM2 (IDX-CMB)
                                   TO  WRK-CMB-ITM2
                   MOVE    SPA-GMN-I01-TOKNYUIN-ITM3 (IDX-CMB)
                                   TO  WRK-CMB-ITM3
                   MOVE    SPA-GMN-I01-TOKNYUIN-ITM4 (IDX-CMB)
                                   TO  WRK-CMB-ITM4
                   MOVE    SPA-GMN-I01-TOKNYUIN-ITM5 (IDX-CMB)
                                   TO  WRK-CMB-ITM5
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-TOKNYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院コンボチェック処理２
      *    実際のコードでチェック
      *****************************************************************
       4103-CMB-TOKNYUIN-SRH2-SEC  SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-TOKNYUIN-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-TOKNYUIN-ITM2 (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-TOKNYUIN-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
                   MOVE    SPA-GMN-I01-TOKNYUIN-ITM2 (IDX-CMB)
                                   TO  WRK-CMB-ITM2
                   MOVE    SPA-GMN-I01-TOKNYUIN-ITM3 (IDX-CMB)
                                   TO  WRK-CMB-ITM3
                   MOVE    SPA-GMN-I01-TOKNYUIN-ITM4 (IDX-CMB)
                                   TO  WRK-CMB-ITM4
                   MOVE    SPA-GMN-I01-TOKNYUIN-ITM5 (IDX-CMB)
                                   TO  WRK-CMB-ITM5
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-TOKNYUIN-SRH2-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料非該当コンボチェック処理
      *****************************************************************
       4103-CMB-TOKHIGAITO-CHK-SEC         SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-TOKHIGAITO-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-TOKHIGAITOL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-TOKHIGAITO-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-TOKHIGAITO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    加算コンボチェック処理
      *****************************************************************
       4103-CMB-KSN-CHK-SEC         SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           MOVE    SPA-GMN-I01-KSN-CNT (IDX-KSN)
                                   TO  WRK-CNT
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   WRK-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-KSNL (IDX-KSN IDX-CMB)
                                   =   WRK-CMB-CD )
      *
      **             IF      SPA-GMN-I01-KSN-SRYCD(IDX-KSN) = "190135470"
      **                 IF    SYS-5000-IPPANKSNKBN   =   SPACE  OR  "0"
      **                     MOVE    1      TO     IDX-CMB
      **                 ELSE
      **                     MOVE    2      TO     IDX-CMB
      **                 END-IF
      **             END-IF
      *
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-KSN-ITM (IDX-KSN IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-KSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求コンボチェック処理
      *****************************************************************
       4103-CMB-SEIKYU-CHK-SEC         SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-SEIKYU-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-SEIKYUL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-SEIKYU-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-SEIKYU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索時患者表示コンボチェック処理
      *****************************************************************
       4103-CMB-DISPKBN-CHK-SEC    SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-DISPKBN-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-DISPKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-DISPKBN-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-DISPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザープログラムコンボチェック処理
      *****************************************************************
       4103-CMB-USERPG-CHK-SEC    SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-USERPG-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-USERPGL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-USERPG-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-USERPG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           EVALUATE    SPA-I0-MOTOPG
           WHEN    "I20"
           WHEN    "K02N"
               MOVE    SPA-I0-MOTOPG   TO  SPA-SAKIPG
           WHEN    OTHER
               MOVE    "M01"           TO  SPA-SAKIPG
           END-EVALUATE
      *
           MOVE    "I01"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  SPA-WORK 
           INITIALIZE                      SPA-KYOTUKEY
                                           SPA-KYOTU-SET
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC                   SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           INITIALIZE    SPA-GMN-I01-HEAD-AREA
                         SPA-NAI-I01-HEAD-AREA
                         SPA-GMN-I01-NYUREKILST-AREA
                         SPA-NAI-I01-NYUREKILST-AREA
                         SPA-GMN-I01-ERRCD-TBL
                         SPA-GMN-I01-CMB-BTUNAME
                         SPA-GMN-I01-CMB-HKNCOMBI
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           PERFORM 310-CMB-SYORIKBN-SET1-SEC
      *
           MOVE    1                   TO  SPA-GMN-I01-CUR
      *
      *
           .
      *
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回患者設定処理
      *****************************************************************
       420-MAEPTINF-SET-SEC            SECTION.
      *
      *    前回患者が設定されているばあは前回患者を表示する
           IF    ( SPA-LAST-PTNUM  NOT =   SPACE )
               MOVE    SPACE           TO  SPA-GMN-I01-ERRCD-TBL
               MOVE    SPA-LAST-PTID   TO  SPA-NAI-I01-PTID
               MOVE    SPA-LAST-PTNUM  TO  SPA-GMN-I01-PTNUM
               PERFORM 4101-PTINF-SELECT-SEC
               IF    ( SPA-ERRCD       =   "9999" )
                   CONTINUE
               ELSE
                   MOVE    SPACE       TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
      *
       420-MAEPTINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力領域初期化処理
      *****************************************************************
       4201-INPUT-AREA-CLEAR-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-I01-INPUT-AREA
                                   SPA-NAI-I01-INPUT-AREA
                                   SPA-GMN-I01-CMB-BTUNAME
                                   SPA-GMN-I01-CMB-HKNCOMBI
                                   SPA-GMN-I01-CMB-TOKNYUIN
                                   SPA-GMN-I01-CMB-KSN
                                   SPA-GMN-I01-CMB-TOKHIGAITO
      *
           MOVE    SPA-SYSYMD      TO  WRK-KJNYMD
      *
           PERFORM 310-CMB-SKJKBN-SET-SEC
      *
           MOVE    "5000"          TO  WRK-KANRICD
           MOVE    "*"             TO  WRK-KBNCD
           MOVE    SPA-SYSYMD      TO  WRK-YUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5000-REC
      *
           MOVE    SPA-GMN-I01-SHOKAI-ITM (01)
                                       TO  SPA-GMN-I01-SHOKAI
           MOVE    SPA-GMN-I01-KAIGO-ITM (01)
                                       TO  SPA-GMN-I01-KAIGO
           MOVE    SPACE               TO  SPA-GMN-I01-BUNBEN
           MOVE    SPACE               TO  SPA-GMN-I01-RIYOU
           MOVE    SPA-GMN-I01-SEIKYU-ITM (01)
                                   TO  SPA-GMN-I01-SEIKYU
           MOVE    SPA-GMN-I01-DISPKBN-ITM (1)
                                   TO  SPA-GMN-I01-DISPKBN
      *
           MOVE    SYS-5000-SKJKBN TO  WRK-CMB-CD
           PERFORM 4103-CMB-SKJKBN-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SKJKBN
           ELSE
               MOVE    "1"             TO  WRK-CMB-CD
               PERFORM 4103-CMB-SKJKBN-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SKJKBN
           END-IF
           MOVE    SYS-5000-SENTEIKBN TO  WRK-CMB-CD
           PERFORM 4103-CMB-SENTEIKBN-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SENTEIKBN
           ELSE
               MOVE    "1"             TO  WRK-CMB-CD
               PERFORM 4103-CMB-SENTEIKBN-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SENTEIKBN
           END-IF
      *
           PERFORM 310-CMB-USERPG-SET-SEC
      *
      *    入院日にシステム日付を表示
           MOVE    SPA-SYSYMD      TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-NYUINYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-I01-NYUINYMD
                                       SPA-NAI-I01-NYUINYMD-MAE
      *
           PERFORM 310-CMB-TIMEKBN-SET-SEC
      *
           MOVE    SPACE           TO  WRK-CMB-CD
           PERFORM 4103-CMB-TIMEKBN-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TIMEKBN
      *
           MOVE    ZERO            TO  SPA-GMN-I01-NYUREKILST-SEL
      *
           MOVE    SPA-GMN-I01-NYUCALKBN-ITM (2)
                                   TO  SPA-GMN-I01-NYUCALKBN
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "01" )
               MOVE    SPACE       TO  SPA-GMN-I01-MAEIDOYMD
                                       SPA-NAI-I01-MAEIDOYMD
               MOVE    ZERO        TO  SPA-NAI-I01-PTRRKNUM
                                       SPA-NAI-I01-PTRRKEDANUM
           END-IF
      *
           .
      *
       4201-INPUT-AREA-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録へ
      *****************************************************************
       480-KANJATOROKU-SEC             SECTION.
      *
           MOVE    SPA-NAI-I01-PTID    TO  SPA-PTID
           MOVE    SPA-GMN-I01-PTNUM   TO  SPA-PTNUM
           PERFORM 481-KANJATOROKU-SEC
      *
          .
       480-KANJATOROKU-EXT.
           EXIT.
      *****************************************************************
      *    患者登録へ
      *****************************************************************
       481-KANJATOROKU-SEC             SECTION.
      *
      *    患者登録の権限がない時、エラー
           IF      SPA-GSRAUTH-12      =   "1"
               CONTINUE
           ELSE
               MOVE    "9300"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-I01-CUR
               GO  TO  481-KANJATOROKU-EXT
           END-IF
      *
      *    ＳＰＡの内容を退避
           INITIALIZE                  I0SUB01-LNK
           MOVE    SPA-TERMID          TO  I0SUB01-TERMID
           MOVE    "O"                 TO  I0SUB01-KBN
           MOVE    SPA-I0FREE          TO  I0SUB01-SCRSPA-AREA
           MOVE    SPA-I0KYOUTU        TO  I0SUB01-COMMON
      *
           CALL    "ORCGI0SUB01"       USING   I0SUB01-LNK
                                               SPA-AREA
      *
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "I01"               TO  SPA-MOTOPG2
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       481-KANJATOROKU-EXT.
           EXIT.
      *****************************************************************
      *    入院歴画面遷移処理
      *****************************************************************
       480-NYUINREKI-SEC               SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 4101-PTNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  480-NYUINREKI-EXT
           END-IF
      *
      *    選択番号未入力チェック
           IF    ( SPA-GMN-I01-SELNUM  =   ZERO )
               MOVE    "1019"      TO  SPA-ERRCD
               MOVE    100         TO  SPA-GMN-I01-CUR
               GO  TO  480-NYUINREKI-EXT
           END-IF
      *
      *    選択番号チェック（基本チェック）
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-NYUINREKI-EXT
           END-IF
      *
           MOVE    SPA-NAI-I01-PTID
                                   TO  SPA-I01-I02-PTID
           MOVE    SPA-NAI-I01-TRRKNUM (SPA-GMN-I01-SELNUM)
                                   TO  SPA-I01-I02-PTRRKNUM
      *
           MOVE    SPACE           TO  SPA-MOTOPG
      *
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I0FREE      TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGI02"      USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-I0FREE
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           IF    ( SPA-NAI-I01-TZITA-FLG (SPA-GMN-I01-SELNUM)
                                   =   ZERO  )
               MOVE    "NYUGAI"        TO  MCP-WIDGET
           ELSE
               MOVE    "B01"           TO  MCP-WIDGET
           END-IF
      *
           MOVE    "I01"           TO  SPA-MOTOPG
           MOVE    "I02"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "I02"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1             TO  FLG-END
      *
           .
       480-NYUINREKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院歴作成画面遷移処理（作成処理）
      *****************************************************************
       480-NYUINRRK-SAKUSEI-SEC        SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 4101-PTNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  480-NYUINRRK-SAKUSEI-EXT
           END-IF
      *
           MOVE    "1"                 TO  SPA-I01-I03-SYORIKBN
           MOVE    SPA-NAI-I01-PTID    TO  SPA-I01-I03-PTID
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
               COMPUTE     SPA-I01-I03-PTRRKNUM    =   1
           ELSE
               COMPUTE     SPA-I01-I03-PTRRKNUM    =   PTNYUINRRK-RRKNUM
                                                   +   1
           END-IF
      *
           MOVE    SPA-GMN-I01-SYORIKBN-CD
                                   TO  SPA-I01-I03-SYORIKBN-CD
      *
           MOVE    SPA-GMN-I01-NYUREKILST-SEL
                                   TO  IDX1
           IF    ( IDX1            >   ZERO )
            AND  ( IDX1            <=  SPA-GMN-I01-NYUREKILST-CNT )
               MOVE    SPA-GMN-I01-TSHONUM (IDX1)
                                   TO  SPA-I01-I03-SHONUM
           ELSE
               MOVE    ZERO        TO  SPA-I01-I03-SHONUM
           END-IF
      *
           MOVE    ZERO            TO  SPA-I01-I03-TAIINYMD
           IF    ( SPA-I01-I03-SHONUM      NOT =   ZERO )
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL     ( IDX2    >    SPA-GMN-I01-NYUREKILST-CNT )
                   IF    ( SPA-I01-I03-SHONUM
                                   =   SPA-GMN-I01-TSHONUM (IDX2) )
                       MOVE    SPA-NAI-I01-TTAIINYMD (IDX2)
                                   TO  SPA-I01-I03-TAIINYMD
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I0FREE      TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGI03"      USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-I0FREE
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           MOVE    "SHOKAI"        TO  MCP-WIDGET
      *
           MOVE    "I01"           TO  SPA-MOTOPG
           MOVE    "I03"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "I03"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1             TO  FLG-END
      *
           .
       480-NYUINRRK-SAKUSEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院歴作成画面遷移処理（修正処理）
      *****************************************************************
       480-NYUINRRK-SYUSEI-SEC         SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    歴が選択されていること
           IF    ( SPA-GMN-I01-NYUREKILST-SEL >   ZERO )
            AND  ( SPA-GMN-I01-NYUREKILST-SEL
                              <=   SPA-GMN-I01-NYUREKILST-CNT )
               CONTINUE
           ELSE
               MOVE    "1023"          TO  SPA-ERRCD
               GO  TO  480-NYUINRRK-SYUSEI-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-NYUREKILST-SEL
                                       TO  IDX1
      *
      *    歴作成データが選択されていること
           IF    ( SPA-NAI-I01-TZITA-FLG (IDX1)    =   ZERO )
               MOVE    "1023"          TO  SPA-ERRCD
               GO  TO  480-NYUINRRK-SYUSEI-EXT
           END-IF
      *
      *    歴作成データが最新であること
           MOVE    ZERO                TO  FLG-RRKCHK
           COMPUTE IDX3    =   IDX1    -   1
           PERFORM VARYING IDX2    FROM    IDX3    BY  -1
                   UNTIL ( IDX2    <   1 )
                   OR    ( FLG-RRKCHK      NOT =   ZERO )
               IF    ( SPA-GMN-I01-TSHONUM (IDX1)
                                   =   SPA-GMN-I01-TSHONUM (IDX2) )
                   MOVE    1       TO  FLG-RRKCHK
                   MOVE    1       TO  IDX2
               END-IF
           END-PERFORM
      *
           IF    ( FLG-RRKCHK       =   ZERO )
               MOVE    "2"             TO  SPA-I01-I03-SYORIKBN
           ELSE
               MOVE    "3"             TO  SPA-I01-I03-SYORIKBN
           END-IF
      *
           MOVE    SPA-NAI-I01-PTID
                                   TO  SPA-I01-I03-PTID
           MOVE    SPA-NAI-I01-TRRKNUM (IDX1)
                                   TO  SPA-I01-I03-PTRRKNUM
           MOVE    SPA-GMN-I01-SYORIKBN-CD
                                   TO  SPA-I01-I03-SYORIKBN-CD
           MOVE    ZERO            TO  SPA-I01-I03-TAIINYMD
           IF    ( SPA-GMN-I01-TSHOKAI (IDX1)  NOT =   "1" )
               COMPUTE IDX3    =   IDX1    +   1
               PERFORM VARYING IDX2    FROM    IDX3    BY  1
                   UNTIL     ( IDX2    >    SPA-GMN-I01-NYUREKILST-CNT )
                   IF    ( SPA-GMN-I01-TSHONUM (IDX1)
                                   =   SPA-GMN-I01-TSHONUM (IDX2) )
                       MOVE    SPA-NAI-I01-TTAIINYMD (IDX2)
                                   TO  SPA-I01-I03-TAIINYMD
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I0FREE      TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGI03"      USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-I0FREE
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           MOVE    "COMMENT"       TO  MCP-WIDGET
      *
           MOVE    "I01"           TO  SPA-MOTOPG
           MOVE    "I03"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "I03"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1             TO  FLG-END
      *
           .
       480-NYUINRRK-SYUSEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索処理
      *****************************************************************
       480-KENSAKU-SEC                 SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-I0FREE          TO  LNK-SCRSPA
      *
           MOVE    ZERO                TO  SPA-PTID
      *
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *    氏名をクリアする
           MOVE    SPACE               TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
      *
       480-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    退院証明書発行処理
      *****************************************************************
       480-TAIIN-PRT-SEC              SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-TAIIN-PRT-EXT
           END-IF
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"  USING   SPA-AREA
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
             OR  ( SPA-PRTCONF         =   "1" )
               INITIALIZE                  ORCSPRTCTRLAREA
               CALL    "ORCSPRTCTRL"       USING
                                           SPA-AREA
                                           ORCSPRTCTRLAREA
                                           MCPAREA
           END-IF
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           MOVE    SPA-AREA        TO  WRK-SPA-AREA
      *
           MOVE    "1"             TO  WRK-SPA-NYUGAIKBN
      *
           MOVE    PTINF-PTID      TO  WRK-SPA-PTID
           MOVE    SPA-GMN-I01-PTNUM
                                   TO  WRK-SPA-PTNUM
           MOVE    PTINF-KANANAME  TO  WRK-SPA-KANANAME
           MOVE    PTINF-NAME      TO  WRK-SPA-NAME
           MOVE    PTINF-SEX       TO  WRK-SPA-SEX
           MOVE    PTINF-BIRTHDAY  TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  WRK-SPA-BIRTHDAYW
           MOVE    WRK-SYMD        TO  WRK-SPA-BIRTHDAY
           MOVE    "ORCHCN02"      TO  WRK-ORG-PRINT-PG
           MOVE    "00000002"      TO  WRK-KBNCD
           PERFORM 4801-PRINT-PG-EDIT-SEC
      *
           PERFORM 4901-PTNYUINRRK-SEL-SEC
      *
           IF    ( PTNYUINRRK-RRKNUM   =   SPA-NAI-I01-PTRRKNUM )
            AND  ( PTNYUINRRK-TAIINYMD =   "99999999" )
               MOVE    SPA-NAI-I01-TAIINYMD
                                   TO  PTNYUINRRK-TAIINYMD
           END-IF
      *
           MOVE    PTNYUINRRK-TAIINYMD
                                   TO  WRK-SPA-SRYYMD
      *
           MOVE    PTNYUINRRK-TAIINYMD (1 : 6)
                                       TO  WRK-NACCT-SRYYM
           PERFORM 4801-NYUINACCT-SEL-SEC
           IF    ( FLG-NYUINACCTARI    =   ZERO )
               MOVE    "1030"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    "01"            TO      WRK-SPA-PRT-FLG
               CALL    WRK-PRINT-PG    USING   WRK-SPA-AREA
                                               PTNYUINRRK-REC
               MOVE    SPACE           TO      WRK-SPA-PRT-FLG
           END-IF
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
      *
           .
       480-TAIIN-PRT-EXT.
           EXIT.
      *****************************************************************
      *    カルテ発行処理
      *****************************************************************
       480-KARUTE-PRT-SEC              SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-KARUTE-PRT-EXT
           END-IF
      *
           MOVE    "2"                 TO  WRK-ORCHCN01-SYORIKBN
           MOVE    "1"                 TO  WRK-ORCHCN01-PRTKBN
      *
      *    入院カルテ編集サブ呼出
           PERFORM 4801-ORCHCN01-CALL-SEC
      *
           .
       480-KARUTE-PRT-EXT.
           EXIT.
      *****************************************************************
      *    続紙発行処理
      *****************************************************************
       480-ZOKUSI-PRT-SEC              SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-ZOKUSI-PRT-EXT
           END-IF
      *
           MOVE    "1"                 TO  WRK-ORCHCN01-SYORIKBN
           MOVE    "2"                 TO  WRK-ORCHCN01-PRTKBN
      *
      *    入院カルテ編集サブ呼出
           PERFORM 4801-ORCHCN01-CALL-SEC
      *
           .
       480-ZOKUSI-PRT-EXT.
           EXIT.
      *****************************************************************
      *    入院診療禄サブ呼出処理
      *****************************************************************
       4801-ORCHCN01-CALL-SEC          SECTION.
      *
           MOVE    SPACE               TO  SPA-PRT-FLG
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"  USING   SPA-AREA
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
             OR  ( SPA-PRTCONF         =   "1" )
               INITIALIZE                  ORCSPRTCTRLAREA
               CALL    "ORCSPRTCTRL"       USING
                                           SPA-AREA
                                           ORCSPRTCTRLAREA
                                           MCPAREA
           END-IF
      *
      *    IF    ( SPA-CLIENT-PRT-FLG  =   1 )
               MOVE     2              TO  SPA-CLIENT-TEMP-FLG
               MOVE    "02"            TO  SPA-PRT-FLG
      *    END-IF
      *
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
           INITIALIZE                      WRK-SPA-KYOTUKEY
                                           WRK-SPA-KYOTU-SET
                                           ORCHCN01AREA
      *
           MOVE    WRK-ORCHCN01-SYORIKBN
                                       TO  ORCHCN01-SYORIKBN
           MOVE    WRK-ORCHCN01-PRTKBN
                                       TO  ORCHCN01-PRTKBN
           MOVE    SPA-NAI-I01-PTID    TO  WRK-SPA-PTID
           MOVE    SPA-GMN-I01-PTNUM   TO  WRK-SPA-PTNUM
           MOVE    SPA-GMN-I01-NYUINKA-CD
                                       TO  WRK-SPA-SRYKA
      *
           MOVE    SPA-SYSYMD          TO  WRK-SPA-SRYYMD
      *
      *    カルテ編集用保険組合せ決定処理
           PERFORM 4801-HKNCOMBI-KETTEI-SEC
      *    MOVE    SPA-GMN-I01-HKNCOMBI-CD
      *                                TO  WRK-SPA-HKNCOMBINUM
           IF    ( WRK-ORCHCN01-HKNCOMBINUM    =   ZERO )
               MOVE    SPACE           TO  WRK-SPA-HKNCOMBINUM
           ELSE
               MOVE    WRK-ORCHCN01-HKNCOMBINUM
                                       TO  WRK-SPA-HKNCOMBINUM
           END-IF
      *
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   3 )
               IF    ( SPA-GMN-I01-DR-CD (IDX1)
                                   NOT =   SPACE )
                   COMPUTE IDX2    =   IDX2    +   1
                   MOVE    "1"         TO  ORCHCN01-DR (IDX2) (1 : 1)
                   MOVE    SPA-GMN-I01-DR-CD (IDX1)
                                       TO  ORCHCN01-DR (IDX2) (2 : )
               END-IF
           END-PERFORM
      *
           MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  ORCHCN01-NYUINYMD
           MOVE    SPA-NAI-I01-TAIINYMD
                                       TO  ORCHCN01-TAIINYMD
      *
      *    DISPLAY "ORCHCN01-SYORIKBN  = " ORCHCN01-SYORIKBN
      *    DISPLAY "ORCHCN01-PRTKBN    = " ORCHCN01-PRTKBN
      *    DISPLAY "SPA-PTID           = " SPA-PTID
      *    DISPLAY "SPA-PTNUM          = " SPA-PTNUM
      *    DISPLAY "SPA-SRYKA          = " SPA-SRYKA
      *    DISPLAY "SPA-HKNCOMBINUM    = " SPA-HKNCOMBINUM
      *    DISPLAY "ORCHCN01-DR (1)    = " ORCHCN01-DR (1)
      *    DISPLAY "ORCHCN01-DR (2)    = " ORCHCN01-DR (2)
      *    DISPLAY "ORCHCN01-DR (3)    = " ORCHCN01-DR (3)
      *    DISPLAY "ORCHCN01-NYUINYMD  = " ORCHCN01-NYUINYMD
      *
      *    帳票プログラム名取得
           MOVE    "ORCHCN01"          TO  WRK-ORG-PRINT-PG
           MOVE    "00000001"          TO  WRK-KBNCD
           PERFORM 4801-PRINT-PG-EDIT-SEC
      *
           CALL    WRK-PRINT-PG    USING   WRK-SPA-AREA
                                           ORCHCN01AREA
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
           MOVE     ZERO                   TO  SPA-CLIENT-TEMP-FLG
      *
           .
       4801-ORCHCN01-CALL-EXT.
           EXIT.
      *****************************************************************
      *   カルテ編集用保険組合せ決定処理
      *****************************************************************
       4801-HKNCOMBI-KETTEI-SEC        SECTION.
      *
           INITIALIZE                  WRK-ORCHCN01-HKNCOMBINUM
      *
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
           WHEN    "01"
               MOVE    SPA-GMN-I01-HKNCOMBI-CD
                                   TO  WRK-ORCHCN01-HKNCOMBINUM
           WHEN    "08"
               IF    ( SPA-GMN-I01-HKNCOMBI-CD
                               NOT =   SPA-NAI-I01-HKNCOMBI-MOT )
                   MOVE    SPA-GMN-I01-HKNCOMBI-CD
                                   TO  WRK-ORCHCN01-HKNCOMBINUM
               ELSE
                   PERFORM 4801-NYUACCT-HKNCOMBI-GET-SEC
               END-IF
           WHEN    OTHER
                   PERFORM 4801-NYUACCT-HKNCOMBI-GET-SEC
           END-EVALUATE
      *
           IF    ( WRK-ORCHCN01-HKNCOMBINUM    =   999 )
               MOVE    ZERO        TO  WRK-ORCHCN01-HKNCOMBINUM
           END-IF
      *
           .
       4801-HKNCOMBI-KETTEI-EXT.
           EXIT.
      *****************************************************************
      *   入院会計より保険組合せ取得処理
      *****************************************************************
       4801-NYUACCT-HKNCOMBI-GET-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           MOVE    ZERO                TO  FLG-HKNCOMBIGET
      *
           IF    ( SPA-NAI-I01-NYUINYMD    <   SPA-SYSYMD )
               MOVE    SPA-SYSYMD      TO  WRK-MIN-YMD
           ELSE
               MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  WRK-MIN-YMD
           END-IF
      *
           INITIALIZE  NYUINACCT-REC
           MOVE    SPA-HOSPNUM          TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SPA-NAI-I01-PTID    TO  NACCT-PTID
           MOVE    WRK-MIN-YM          TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-NYUINACCT
           ELSE
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-NYUINACCT   =   1 )
                   OR    ( FLG-HKNCOMBIGET =   1 )
                   OR    ( NACCT-SRYYM <   SPA-NAI-I01-NYUINYMD (1 : 6))
      *
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    NACCT-SRYYM         TO  LNK-DAY7-YM
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI(7:2)
                                           TO  WRK-STIDX
      *
               MOVE    1                   TO  WRK-EDIDX
      *
               PERFORM VARYING IDX-DAY   FROM    WRK-STIDX   BY  -1
                   UNTIL ( IDX-DAY           <   WRK-EDIDX )
                    OR   ( FLG-HKNCOMBIGET =   1 )
      *
                   IF    ( WRK-MIN-YM      =   NACCT-SRYYM )
                       IF    ( WRK-MIN-DD >=  IDX-DAY)
                        AND  ( NACCT-DAY (IDX-DAY)   NOT =   ZERO )
                           MOVE    NACCT-DAY (IDX-DAY)
                                           TO  WRK-ORCHCN01-HKNCOMBINUM
                           MOVE    1       TO  FLG-HKNCOMBIGET
                       END-IF
                   ELSE
                       MOVE    SPA-NAI-I01-NYUINYMD
                                           TO  WRK-NAI-NYUINYMD
                       IF    ( WRK-NAI-NYUINYM
                                           =   NACCT-SRYYM )
                        AND  ( WRK-NAI-NYUINDD
                                           >   IDX-DAY)
                           MOVE    1       TO  IDX-DAY
                       ELSE
                           IF    ( NACCT-DAY (IDX-DAY)   NOT =   ZERO )
                               MOVE    NACCT-DAY (IDX-DAY)
                                           TO  WRK-ORCHCN01-HKNCOMBINUM
                               MOVE    1   TO  FLG-HKNCOMBIGET
                           END-IF
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key19"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    1           TO  FLG-NYUINACCT
               ELSE
                   MOVE    MCPDATA-REC TO  NYUINACCT-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4801-NYUACCT-HKNCOMBI-GET-EXT.
           EXIT.
      *****************************************************************
      *   帳票プログラム名編集処理
      *****************************************************************
       4801-PRINT-PG-EDIT-SEC         SECTION.
      *
           INITIALIZE                      WRK-PRINT-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    WRK-KBNCD           TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYS-1035-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG  TO  WRK-PRINT-PG
           ELSE
               MOVE   WRK-ORG-PRINT-PG TO  WRK-PRINT-PG
           END-IF
      *
           .
       4801-PRINT-PG-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院会計照会遷移処理
      *****************************************************************
       480-NYUINKAIKEI-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    入院会計照会へ引き継ぐ情報を設定
           PERFORM 4801-SENIJI-PARA-SET-SEC
      *
      *    ＳＰＡの内容を退避
           INITIALIZE                  I0SUB01-LNK
           MOVE    SPA-TERMID          TO  I0SUB01-TERMID
           MOVE    "O"                 TO  I0SUB01-KBN
           MOVE    SPA-I0FREE          TO  I0SUB01-SCRSPA-AREA
           MOVE    SPA-I0KYOUTU        TO  I0SUB01-COMMON
      *
           CALL    "ORCGI0SUB01"   USING   I0SUB01-LNK
                                           SPA-AREA
      *
      *    入院会計照会へ遷移する
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "I41"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-I0KYOUTU        TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-I0FREE          TO  LNK-SCRSPA
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "I41"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-NYUINKAIKEI-EXT.
           EXIT.
      *****************************************************************
      *    出産一時金画面遷移処理
      *****************************************************************
       480-S08-SENI-SEC                SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF    ( SPA-GMN-I01-NYUREKILST-SEL >   ZERO )
            AND  ( SPA-GMN-I01-NYUREKILST-SEL
                              <=   SPA-GMN-I01-NYUREKILST-CNT )
      *
               MOVE    SPA-NAI-I01-PTID    TO  SPA-PTID
               MOVE    SPA-GMN-I01-PTNUM   TO  SPA-PTNUM
               MOVE    "1"                 TO  SPA-NYUGAIKBN
      *
               INITIALIZE                  LNKS08-AREA
               MOVE    "I01"               TO  LNKS08-SAKIPG
               MOVE    SPA-NAI-I01-TRRKNUM (SPA-GMN-I01-NYUREKILST-SEL)
                                           TO  LNKS08-RRKNUM
               MOVE    LNKS08-AREA         TO  SPA-WORK
               MOVE    "I01"               TO  SPA-MOTOPG
               MOVE    "S08"               TO  SPA-SAKIPG
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
               MOVE    "I01"               TO  SPA-I0-MOTOPG2
      *
      *        ＳＰＡの内容を退避
               INITIALIZE                  I0SUB01-LNK
               MOVE    SPA-TERMID          TO  I0SUB01-TERMID
               MOVE    "O"                 TO  I0SUB01-KBN
               MOVE    SPA-I0FREE          TO  I0SUB01-SCRSPA-AREA
               MOVE    SPA-I0KYOUTU        TO  I0SUB01-COMMON
      *
               CALL    "ORCGI0SUB01"   USING   I0SUB01-LNK
                                               SPA-AREA
      *
      *        出産一時金画面へ遷移する
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               MOVE   "S02"                TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       480-S08-SENI-EXT.
           EXIT.
      *****************************************************************
      *    収納登録遷移処理
      *****************************************************************
       480-S02-SENI-SEC                SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    収納登録へ引き継ぐ情報を設定
           PERFORM 4801-SENIJI-PARA-SET-SEC
      *
      *    ＳＰＡの内容を退避
           INITIALIZE                  I0SUB01-LNK
           MOVE    SPA-TERMID          TO  I0SUB01-TERMID
           MOVE    "O"                 TO  I0SUB01-KBN
           MOVE    SPA-I0FREE          TO  I0SUB01-SCRSPA-AREA
           MOVE    SPA-I0KYOUTU        TO  I0SUB01-COMMON
      *
           CALL    "ORCGI0SUB01"       USING   I0SUB01-LNK
                                               SPA-AREA
      *
      *    収納登録へ遷移する
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "S02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-I0KYOUTU        TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-I0FREE          TO  LNK-SCRSPA
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "S02"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-S02-SENI-EXT.
           EXIT.
      *****************************************************************
      *    オーダー変更画面遷移処理
      *****************************************************************
       480-I06-SENI-SEC                SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "I06"               TO  SPA-SAKIPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "I06"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-I06-SENI-EXT.
           EXIT.
      *****************************************************************
      *    他業務引継情報設定処理
      *****************************************************************
       4801-SENIJI-PARA-SET-SEC        SECTION.
      *
           INITIALIZE                  SPA-KYOTUKEY
                                       SPA-KYOTU-SET
      *
      *    入院科を編集
           MOVE    SPACE           TO  WRK-SRYKAMEI
      *
           IF    ( SPA-GMN-I01-NYUINKA NOT =   SPACE )
               MOVE    SPA-GMN-I01-NYUINKA-CD
                                   TO  WRK-CMB-CD
               PERFORM 4103-CMB-NYUINKA-CHK-SEC
               MOVE    WRK-CMB-ITM
                                   TO  WRK-SRYKAMEI
           END-IF
      *
      *    システム日付を診療日付として会計に引き継ぐ
           MOVE    SPA-SYSYMD      TO  SPA-SRYYMD
           MOVE    SPA-SYSYMDWH    TO  SPA-SRYYMDW
      *
      *    入院科が入力されていた場合は会計に引き継ぐ
      *    （患者情報が入力されていた場合に限る）
           IF    ( WRK-SRYKAMEI        NOT =   SPACE )
            AND  ( SPA-NAI-I01-PTID    NOT =   ZERO )
               MOVE    SPA-GMN-I01-NYUINKA-CD
                                   TO  SPA-SRYKA
               MOVE    WRK-SRYKAMEI
                                   TO  SPA-SRYKAMEI
           END-IF
      *
      *    患者情報が入力されていた場合は会計に引継ぐ
           IF    ( SPA-NAI-I01-PTID    NOT =   ZERO )
               MOVE    SPA-NAI-I01-PTID
                                   TO  SPA-PTID
               MOVE    SPA-GMN-I01-PTNUM
                                   TO  SPA-PTNUM
               MOVE    SPA-NAI-I01-KANANAME
                                   TO  SPA-KANANAME
               MOVE    SPA-GMN-I01-NAME
                                   TO  SPA-NAME
               MOVE    SPA-GMN-I01-SEX
                                   TO  SPA-SEX
               MOVE    SPA-GMN-I01-BIRTHDAY
                                   TO  SPA-BIRTHDAYW
               MOVE    SPA-NAI-I01-BIRTHDAY
                                   TO  SPA-BIRTHDAY
           END-IF
      *
           .
       4801-SENIJI-PARA-SET-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       480-TOUROKU-SEC                 SECTION.
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD  = "02" OR "10"  OR "11")
            AND  ( FLG-TEIKI-CHK       =   1 )
      *
      *        定期請求処理中チェック
               PERFORM 4801-TEIKI-JOB-CHK-SEC
               IF    ( SPA-I0IDCD      NOT =   SPACE )
                   GO  TO  480-TOUROKU-EXT
               END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
           WHEN    "10"
      *
               PERFORM 4101-GMN-SPA-SET-SEC
      *
               PERFORM 4101-PTNUM-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                 OR  ( FLG-END     NOT =   ZERO  )
                   GO  TO  480-TOUROKU-EXT
               END-IF
           WHEN    "05"
           WHEN    "06"
           WHEN    "07"
           WHEN    "09"
               CONTINUE
           WHEN    OTHER
      *
               PERFORM 410-INPUT-CHK-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  480-TOUROKU-EXT
               END-IF
      *
               PERFORM 4801-KANREN-CHK-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  480-TOUROKU-EXT
               END-IF
           END-EVALUATE
      *
           PERFORM 4801-SYORI-KANREN-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           MOVE    SPACE           TO  WRK-UPDKBN
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
      *    入院登録
           WHEN    "01"
      *
               IF    ( FLG-TAINDOUJITU =   1 )
                   MOVE    "3009"  TO  SPA-I0IDCD
                   GO  TO  480-TOUROKU-EXT
               END-IF
      *
               IF    ( FLG-TAINDOUJITU =   2 )
                   MOVE    "3010"  TO  SPA-I0IDCD
                   GO  TO  480-TOUROKU-EXT
               END-IF
      *
               IF    ( SPA-NAI-I01-MAEIDOYMD   =   SPACE )
                   MOVE    "INS"       TO  WRK-UPDKBN
               ELSE
                   MOVE    "UPD"       TO  WRK-UPDKBN
               END-IF
      *
      *    退院登録
           WHEN    "02"
           WHEN    "11"
      *
               IF    ( FLG-SYUARI  NOT =   ZERO )
                   MOVE    "3005"  TO  SPA-I0IDCD
                   GO  TO  480-TOUROKU-EXT
               END-IF
      *
      *        診療会計テーブルチェック処理
               PERFORM 490-SRYACCT-CHK-SEC
               IF    ( SPA-I0IDCD  NOT =   SPACE )
                 OR  ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  480-TOUROKU-EXT
               END-IF
      *
      *        退院処理
               MOVE    "CHANGE"    TO  WRK-MCP-PUTTYPE
               PERFORM 490-TAIIN-SYORI-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  480-TOUROKU-EXT
               END-IF
               GO  TO  480-TOUROKU-EXT
      *    転科・転棟・転室
           WHEN    "08"
               MOVE    "INS"       TO  WRK-UPDKBN
      *    入院取消
           WHEN    "05"
               IF    ( SPA-NAI-I01-PTRRKEDANUM >   1 )
                   MOVE    "3006"      TO  SPA-I0IDCD
               ELSE
                   MOVE    "3001"      TO  SPA-I0IDCD
               END-IF
               GO  TO  480-TOUROKU-EXT
      *    入院取消（会計含む）
           WHEN    "06"
               IF    ( SPA-NAI-I01-PTRRKEDANUM >   1 )
                   MOVE    "3007"      TO  SPA-I0IDCD
               ELSE
                   MOVE    "3002"      TO  SPA-I0IDCD
               END-IF
               GO  TO  480-TOUROKU-EXT
      *    退院取消
           WHEN    "07"
               MOVE    "3003"      TO  SPA-I0IDCD
               GO  TO  480-TOUROKU-EXT
      *    異動取消
           WHEN    "09"
               MOVE    "DEL"       TO  WRK-UPDKBN
               MOVE    "3101"      TO  SPA-I0IDCD2
               GO  TO  480-TOUROKU-EXT
      *    退院再計算
           WHEN    "10"
               MOVE    "CHANGE"    TO  WRK-MCP-PUTTYPE
               PERFORM 490-TAIIN-SAIKEISAN-SYORI-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  480-TOUROKU-EXT
               END-IF
               GO  TO  480-TOUROKU-EXT
      *    その他
           WHEN    OTHER
               MOVE    "UPD"       TO  WRK-UPDKBN
           END-EVALUATE
      *
           PERFORM 4801-PTNYUINRRK-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           .
      *
       480-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    患者履歴マスタ更新処理
      *****************************************************************
       4801-PTNYUINRRK-UPD-SEC     SECTION.
      *
      *    入院履歴データ編集
           PERFORM 4801-PTNYUINRRK-EDIT-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4801-PTNYUINRRK-UPD-EXT
           END-IF
      *
           MOVE    PTNYUINRRK-REC          TO  NRRK-REC
      *
           EVALUATE WRK-UPDKBN
           WHEN    "INS"
      *        入院履歴マスタ追加
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               PERFORM 910-DBINSERT-SEC
           WHEN    "UPD"
      *        入院履歴マスタ更新
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               PERFORM 910-DBUPDATE-SEC
           WHEN    "DEL"
      *        入院履歴マスタ削除
               IF    ( SPA-GMN-I01-SYORIKBN-CD
                                       =   "05"  OR  "06" )
                   MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
               ELSE
                   MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
               END-IF
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               PERFORM 910-DBDELETE-SEC
           END-EVALUATE
      *
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "4001"      TO  SPA-ERRCD
               GO  TO  4801-PTNYUINRRK-UPD-EXT
           END-IF
      *
           PERFORM 900-NYUINACCT-KEY19-SEL-SEC
           MOVE    NACCT-SRYYM TO  WRK-LASTSRYYM
      *
      *    ユーザプログラム用パラメタ編集処理
           PERFORM 4801-USERPGRRK-EDIT-SEC
      *
      *    PUSH用パラメタ編集処理
           PERFORM 4801-PUSH-EDIT-SEC
      *
           MOVE    SPA-GMN-I01-SYORIKBN-CD TO  WRK-I01-SYORIKBN-CD
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "01" )
            AND  ( SPA-NAI-I01-MAEIDOYMD   NOT =   SPACE )
               MOVE    "08"                TO  SPA-GMN-I01-SYORIKBN-CD
               MOVE    SPA-NAI-I01-MAEIDOYMD
                                           TO  SPA-NAI-I01-IDOYMD
           END-IF
      *
      D    DISPLAY "##SPA-GMN-I01-SYORIKBN-CD =" SPA-GMN-I01-SYORIKBN-CD
      *
      *    入院会計テーブルを編集する（入院時・転室時）
      *     IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01"
      *                                     OR  "08" )
           IF      SPA-GMN-I01-SYORIKBN-CD =   "01"
               PERFORM 4801-NYUKAIKEI-EDIT-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                   MOVE    "abort"         TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   GO  TO  4801-PTNYUINRRK-UPD-EXT
               END-IF
           END-IF
      *
      *    入院会計テーブルを編集する（入院取消時）
           IF     SPA-GMN-I01-SYORIKBN-CD =   "05"  OR  "06"
               PERFORM 4801-NYUKAIKEI-ERASE-SEC
           END-IF
      *
      *    入院会計テーブルを編集する（退院取消時）
           IF     SPA-GMN-I01-SYORIKBN-CD =   "07"
               PERFORM 4801-TAIINKAIKEI-CANCEL-SEC
           END-IF
      *
      *    入院会計テーブルを編集する（転科／転棟／転室時）
           IF     SPA-GMN-I01-SYORIKBN-CD =   "08"
               PERFORM 4801-TENROOM-EDIT-SEC
               IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
                   MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
                   MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
                   MOVE    IPN-SANTEISTYMD   TO   SPA-NAI-I01-IDOYMD
                   PERFORM   4801-TENROOM-EDIT-SEC
               END-IF
           END-IF
      *
      *    入院会計テーブルを編集する（異動取消時）
           IF     SPA-GMN-I01-SYORIKBN-CD =   "09"
               PERFORM 4801-IDOKAIKEI-CANCEL-SEC
           END-IF
      *
           IF  ( WRK-I01-SYORIKBN-CD       =   "01" )
               MOVE   WRK-I01-SYORIKBN-CD  TO  SPA-GMN-I01-SYORIKBN-CD
               MOVE   SPACE                TO  SPA-NAI-I01-IDOYMD
           END-IF
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "01" )
            AND  ( SPA-NAI-I01-MAEIDOYMD   NOT =   SPACE )
            AND  ( SPA-GMN-I01-SKJKBN-CD       =   "A" OR "B" )
               MOVE    NRRK-REC                TO  PTNYUINRRK-REC
               MOVE    SPA-NAI-I01-MAESKJKBN   TO  PTNYUINRRK-SKJKBN
               MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               MOVE    PTNYUINRRK-REC          TO  MCPDATA-REC
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4036"      TO  SPA-ERRCD
                   GO  TO  4801-PTNYUINRRK-UPD-EXT
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD
                                       =   "05"  OR  "06" )
               CONTINUE
           ELSE
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD    TO  PTNYUINRRK-UPYMD
               MOVE    SMCNDATE-HMS    TO  PTNYUINRRK-UPHMS
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "upd4"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4031"      TO  SPA-ERRCD
                   GO  TO  4801-PTNYUINRRK-UPD-EXT
               END-IF
           END-IF
      *
      *    分娩管理テーブル更新処理
           PERFORM 4801-BUNBEN-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4801-PTNYUINRRK-UPD-EXT
           END-IF
      *
      *    患者情報の入院歴番号を更新する
      *    主科情報も更新する
           PERFORM 4801-PTINF-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4801-PTNYUINRRK-UPD-EXT
           END-IF
      *
           PERFORM 4801-ORCSS010-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4801-PTNYUINRRK-UPD-EXT
           END-IF
      *
      *    前回患者情報を設定
           MOVE SPA-NAI-I01-PTID   TO  SPA-LAST-PTID
           MOVE SPA-GMN-I01-PTNUM  TO  SPA-LAST-PTNUM
      *
      *    ユーザープログラム
           PERFORM 4801-USERPG-SEC
      *
      *    PUSH通知
           PERFORM 4801-PUSH-SEC
      *
      *    画面を更新後の内容で再表示
           PERFORM 4100-PTNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4801-PTNYUINRRK-UPD-EXT
           END-IF
      *
      D    DISPLAY "PTRRKNUM AFTER UPD:" SPA-NAI-I01-PTRRKNUM
      *
           .
      *
       4801-PTNYUINRRK-UPD-EXT.
           EXIT.
      *****************************************************************
      *    定期請求処理中チェック
      *****************************************************************
       4801-TEIKI-JOB-CHK-SEC          SECTION.
      *
      *    ジョブ管理チェック処理
           INITIALIZE                  STEIKIJOB-AREA
           CALL    "ORCSTEIKIJOB"      USING   STEIKIJOB-AREA
                                               SPA-AREA
           IF    ( STEIKIJOB-ERRCD NOT =   SPACE )
                   MOVE    "3008"      TO  SPA-I0IDCD
           END-IF
      *
           .
       4801-TEIKI-JOB-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザプログラム用パラメタ編集処理
      *****************************************************************
       4801-USERPGRRK-EDIT-SEC         SECTION.
      *
           INITIALIZE                  USERPGRRK-REC
           IF    ( SPA-GMN-I01-SYORIKBN-CD
                                       =   "05"  OR  "06" OR "09" )
               MOVE    SPA-NAI-I01-MAEIDOYMD   TO  USERPGRRK-TENNYUYMD
               MOVE    SPA-NAI-I01-PTID        TO  USERPGRRK-PTID
               MOVE    SPA-GMN-I01-NYUINKA-CD  TO  USERPGRRK-NYUINKA
               MOVE    SPA-GMN-I01-HKNCOMBI-CD TO  USERPGRRK-HKNCOMBINUM
               IF    ( SPA-GMN-I01-DR-CD (1)   NOT =   SPACE )
                   MOVE    "1"                 TO  USERPGRRK-DRCD1 (1:1)
                   MOVE    SPA-GMN-I01-DR-CD (1)
                                               TO  USERPGRRK-DRCD1 (2:4)
               END-IF
           ELSE
               MOVE    PTNYUINRRK-REC          TO  USERPGRRK-REC
               IF    ( SPA-GMN-I01-SYORIKBN-CD =   "07" )
                   MOVE    SPA-NAI-I01-TAIINYMD TO USERPGRRK-TENNYUYMD
               END-IF
           END-IF
      *
           .
       4801-USERPGRRK-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ユーザプログラム起動処理
      *****************************************************************
       4801-USERPG-SEC                 SECTION.
      *
           MOVE    SPA-GMN-I01-SYORIKBN-CD
                                       TO  WRK-99
           MOVE    WRK-99              TO  SPA-SYORI-FLG
      *
           PERFORM 4801-SENIJI-PARA-SET-SEC
           MOVE    USERPGRRK-DRCD1     TO  SPA-DRCD
           MOVE    USERPGRRK-TENNYUYMD TO  SPA-SRYYMD
           MOVE    USERPGRRK-TENNYUYMD TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG         TO  SPA-SRYYMDW
           MOVE    USERPGRRK-HKNCOMBINUM
                                       TO  SPA-HKNCOMBINUM
      *
           IF    ( SPA-GMN-I01-USERPG-CD   =   "1" )
               PERFORM 48011-XD01-SENI-SEC
           ELSE
               IF    ( SPA-NAI-I01-GYOUMU-ARI  =   "1" )
      *
                   INITIALIZE                      ORCSUSERCHKAREA
                   MOVE    "I01"               TO  ORCSUSERCHK-GMNPG
                   MOVE    "2"                 TO  ORCSUSERCHK-SYORI
                   MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SYSYMD
                   MOVE    SPA-SRYYMD          TO  ORCSUSERCHK-SRYYMD
                   MOVE    SPA-PTID            TO  ORCSUSERCHK-PTID
                   MOVE    SPA-PTNUM           TO  ORCSUSERCHK-PTNUM
                   MOVE    SPA-SRYKA           TO  ORCSUSERCHK-SRYKA
                   MOVE    SPA-HKNCOMBINUM     TO  ORCSUSERCHK-HKNCOMBI
                   MOVE    SPA-DRCD            TO  ORCSUSERCHK-DRCD
                   MOVE    SPA-SYORI-FLG       TO  ORCSUSERCHK-SYORI-FLG
                   CALL    "ORCSUSERCHK"       USING
                                               ORCSUSERCHKAREA
                                               SPA-AREA
               END-IF
           END-IF
      *
           .
       4801-USERPG-EXT.
           EXIT.
      *****************************************************************
      *    ユーザプログラム実行画面表示処理
      *****************************************************************
       48011-XD01-SENI-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
      *    共通キー編集
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "XD01"              TO  SPA-SAKIPG
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
      *    共通のキーをクリアする
           MOVE    SPACE               TO  SPA-ERRMSG
           INITIALIZE                      SPA-KYOTUKEY
      *
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "I01"               TO  SPA-SAKIPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPACE               TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "XD01"              TO  SPA-SAKIPG
      *
           IF    ( WRK-I0IDCD          =   SPACE )
               MOVE    "CHANGE"        TO  MCP-PUTTYPE
           ELSE
               MOVE    "JOIN"          TO  MCP-PUTTYPE
           END-IF
           MOVE    "XD01"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       48011-XD01-SENI-EXT.
           EXIT.
      *****************************************************************
      *    PUSH通知用パラメタ編集処理
      *****************************************************************
       4801-PUSH-EDIT-SEC              SECTION.
      *
           INITIALIZE                      SI03-AREA
      *
           MOVE    SPA-GMN-I01-SYORIKBN-CD
                                       TO  SI03-REQNUM
           MOVE    SPA-GMN-I01-PTNUM   TO  SI03-PTNUM
           MOVE    NRRK-NYUINYMD       TO  SI03-NYUINYMD
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "07" )
               MOVE    SPA-NAI-I01-TAIINYMD
                                       TO  SI03-TAIINYMD
           END-IF
      *
           .
       4801-PUSH-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    PUSH通知処理
      *****************************************************************
       4801-PUSH-SEC                       SECTION.
      *
           CALL    "ORCSI03"           USING
                                       SI03-AREA
                                       SPA-AREA
      *
           .
       4801-PUSH-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4801-KANREN-CHK-SEC         SECTION.
      *
      *    保険組合せが存在すること
      *    IF    ( SPA-GMN-I01-HKNCOMBI-CNT    >   ZERO )
      *        CONTINUE
      *    ELSE
      *        MOVE    "1021"      TO  SPA-ERRCD
      *        MOVE    1           TO  SPA-GMN-I01-CUR
      *        GO  TO  4801-KANREN-CHK-EXT
      *    END-IF
      *
      *    必須入力チェック
      *    病室番号
           IF    ( SPA-GMN-I01-BRMNUM      =   SPACE )
               MOVE    "1003"      TO  SPA-ERRCD
               MOVE    4           TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    病棟名
           IF    ( SPA-GMN-I01-BTUNAME     =   SPACE )
               MOVE    "1004"      TO  SPA-ERRCD
               MOVE    5           TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    入院日
           IF    ( SPA-GMN-I01-NYUINYMD    =   SPACE )
               MOVE    "1005"      TO  SPA-ERRCD
               MOVE    7           TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    入院科
           IF    ( SPA-GMN-I01-NYUINKA     =   SPACE )
               MOVE    "1006"      TO  SPA-ERRCD
               MOVE    8           TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    初回
           IF    ( SPA-GMN-I01-SHOKAI      =   SPACE )
               MOVE    "1007"      TO  SPA-ERRCD
               MOVE    9           TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    初回履歴番号
           IF    ( SPA-GMN-I01-SHOKAI-CD   =   "2" )
               IF    ( SPA-GMN-I01-SHONUM  =   ZERO )
                   MOVE    "1022"      TO  SPA-ERRCD
                   MOVE    10          TO  SPA-GMN-I01-CUR
                   GO  TO  4801-KANREN-CHK-EXT
               END-IF
           END-IF
      *
      *    介護異動
           IF    ( SPA-GMN-I01-KAIGO      =   SPACE )
               MOVE    "1046"      TO  SPA-ERRCD
               MOVE    26          TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    担当医
      ***  IF    ( SPA-GMN-I01-DR (1)         =   SPACE )
      *     AND  ( SPA-GMN-I01-DR (2)         =   SPACE )
      *     AND  ( SPA-GMN-I01-DR (3)         =   SPACE )
      *        MOVE    "1008"      TO  SPA-ERRCD
      *        MOVE    11          TO  SPA-GMN-I01-CUR
      *        GO  TO  4801-KANREN-CHK-EXT
      *    END-IF
      *
      *
      *    保険組合せ
           IF    ( SPA-GMN-I01-HKNCOMBI    =   SPACE )
            AND  ( SPA-GMN-I01-SYORIKBN-CD =   "01" OR "08" )
               MOVE    "1009"      TO  SPA-ERRCD
               MOVE    15          TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *    労災保険（アフター）
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" OR "08" )
             IF    ( SPA-NAI-I01-RSI-HKNKBN    =   "3" )
               MOVE    "1033"      TO  SPA-ERRCD
               MOVE    15          TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
             END-IF
           END-IF
      *
      *    入院料算定
           IF    ( SPA-GMN-I01-NYUCALKBN  =   SPACE )
               MOVE    "1048"      TO  SPA-ERRCD
               MOVE     32         TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    定期請求
           IF    ( SPA-GMN-I01-SEIKYU      =   SPACE )
               MOVE    "1010"      TO  SPA-ERRCD
               MOVE    18          TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    検索時患者表示
           IF    ( SPA-GMN-I01-DISPKBN     =   SPACE )
               MOVE    "1011"      TO  SPA-ERRCD
               MOVE    19          TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    入院基本料チェック
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" OR "08" )
               IF    ( SPA-GMN-I01-TOKNYUIN    =   SPACE )
                   MOVE    "5001"              TO  WRK-KANRICD
                   MOVE    SPA-GMN-I01-BTUNAME-CD
                                               TO  WRK-KBNCD
                   MOVE    WRK-KIHON-KJNYMD    TO  WRK-YUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   MOVE    SYSKANRI-REC        TO  SYS-5001-REC
      *
                   IF    ( SYS-5001-BTU-KHNSRYCD   =   SPACE )
                       MOVE    "1027"          TO  SPA-ERRCD
                       MOVE    16              TO  SPA-GMN-I01-CUR
                       GO  TO  4801-KANREN-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "01"
                                                OR "02"
                                                OR "10"
                                                OR "11"
                                                OR "08" )
               PERFORM 48011-H18-KAISEI-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4801-KANREN-CHK-EXT
               END-IF
           END-IF
      *
           .
       4801-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理別関連チェック処理
      *****************************************************************
       48011-H18-KAISEI-CHK-SEC        SECTION.
      *
           MOVE       ZERO         TO  WRK-CHK-YMD
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
      *    入院登録
           WHEN    "01"
               MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  WRK-CHK-YMD
               MOVE    "入院日に"  TO  WRK-ERRMSG
      *    退院登録
           WHEN    "02"
           WHEN    "10"
           WHEN    "11"
               MOVE    SPA-NAI-I01-TAIINYMD
                                   TO  WRK-CHK-YMD
               MOVE    "退院日に"  TO  WRK-ERRMSG
      *    転科　転棟　転室
           WHEN    "08"
      *
               MOVE    SPA-NAI-I01-IDOYMD
                                   TO  WRK-CHK-YMD
               MOVE    "異動日に"  TO  WRK-ERRMSG
           END-EVALUATE
      *
           MOVE    ZERO                TO  FLG-SYS5001
           MOVE    "5001"              TO  WRK-KANRICD
           MOVE    SPA-GMN-I01-BTUNAME-CD  TO  WRK-KBNCD
           MOVE    WRK-CHK-YMD         TO  WRK-YUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5001-REC
           MOVE    FLG-SYSKANRI        TO  FLG-SYS5001
      *
           IF    ( FLG-SYS5001         NOT =   ZERO )
               MOVE    "1045"          TO  SPA-ERRCD
               GO  TO  48011-H18-KAISEI-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYS5000
           MOVE    "5000"              TO  WRK-KANRICD
           MOVE    "*"                 TO  WRK-KBNCD
           MOVE    WRK-CHK-YMD         TO  WRK-YUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5000-REC
           MOVE    FLG-SYSKANRI        TO  FLG-SYS5000
      *
           IF    ( FLG-SYS5000         NOT =   ZERO )
               MOVE    "1044"          TO  SPA-ERRCD
               GO  TO  48011-H18-KAISEI-CHK-EXT
           END-IF
      *
           IF    ( WRK-CHK-YMD     >=  CONST-H180401 )
      *
               IF    ( SYS-5001-STYUKYMD NOT =  SPACE )
                AND  ( SYS-5001-STYUKYMD   >=  CONST-H180401 )
                   CONTINUE
               ELSE
                   MOVE    "1039"          TO  SPA-ERRCD
                   GO  TO  48011-H18-KAISEI-CHK-EXT
               END-IF
      *
               IF    ( SYS-5000-STYUKYMD NOT =  SPACE )
                 AND ( SYS-5000-STYUKYMD    >=  CONST-H180401 )
                   CONTINUE
               ELSE
                   MOVE    "1040"          TO  SPA-ERRCD
                   GO  TO  48011-H18-KAISEI-CHK-EXT
               END-IF
           END-IF
      *
           IF    ( WRK-CHK-YMD     >=  CONST-H180701 )
      *
               IF    ( SYS-5001-BTU-SBT     =   "04"   OR  "09" )
                   IF    ( SYS-5001-STYUKYMD NOT =  SPACE )
                    AND  ( SYS-5001-STYUKYMD   >=  CONST-H180701 )
                       CONTINUE
                   ELSE
                       MOVE    "1041"          TO  SPA-ERRCD
                       GO  TO  48011-H18-KAISEI-CHK-EXT
                   END-IF
               END-IF
               IF    ( SPA-NAI-I01-MAEIDOYMD
                                           NOT =   SPACE )
                 AND ( SPA-NAI-I01-MAEIDOYMD
                                               <   CONST-H180701 )
                   IF    ( ( SPA-GMN-I01-SYORIKBN-CD
                                               =   "02" )
                       AND ( WRK-CHK-YMD  >=  CONST-H180701 ))
                     OR  ( ( SPA-GMN-I01-SYORIKBN-CD
                                               =   "08" )
                       AND ( WRK-CHK-YMD   >  CONST-H180701 ))
      *
                       MOVE    "5001"              TO  WRK-KANRICD
                       MOVE    SPA-NAI-I01-BTUNAME-MOT (1:2)
                                                   TO  WRK-KBNCD
                       MOVE    SPA-NAI-I01-MAEIDOYMD
                                                   TO  WRK-YUKYMD
                       PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                       MOVE    SYSKANRI-REC        TO  SYS-5001-REC
      *
                       IF    ( SYS-5001-BTU-SBT
                                               =   "04"   OR  "09" )
                           MOVE    "1043"      TO  SPA-ERRCD
                       END-IF
      *
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       48011-H18-KAISEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理別関連チェック処理
      *****************************************************************
       4801-SYORI-KANREN-CHK-SEC       SECTION.
      *
      *    処理区分固有の関連チェック
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
      *    入院登録
           WHEN    "01"
               PERFORM 4801-NYUIN-TOUROKU-CHK-SEC
      *    退院登録
           WHEN    "02"
           WHEN    "11"
               PERFORM 4801-TAIIN-TOUROKU-CHK-SEC
      *    変更
           WHEN    "03"
               PERFORM 4801-HENKOU-CHK-SEC
      *    照会
           WHEN    "04"
               PERFORM 4801-SYOKAI-CHK-SEC
      *    入院取消
           WHEN    "05"
           WHEN    "06"
               PERFORM 4801-NYUIN-TORIKESI-CHK-SEC
      *    退院取消
           WHEN    "07"
               PERFORM 4801-TAIIN-TORIKESI-CHK-SEC
      *    転科　転棟　転室
           WHEN    "08"
               PERFORM 4801-TEKNA-CHK-SEC
      *    異動取消
           WHEN    "09"
               PERFORM 4801-IDO-TORIKESI-CHK-SEC
      *    退院再計算
           WHEN    "10"
               PERFORM 4801-TAIIN-SAIKEISAN-CHK-SEC
           END-EVALUATE
      *
           .
       4801-SYORI-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院登録時関連チェック
      *****************************************************************
       4801-NYUIN-TOUROKU-CHK-SEC      SECTION.
      *
           IF    ( SPA-NAI-I01-BIRTHDAY    >   SPA-NAI-I01-NYUINYMD )
               MOVE    "1047"      TO  SPA-ERRCD
               MOVE    7           TO  SPA-GMN-I01-CUR
               GO  TO  4801-NYUIN-TOUROKU-CHK-EXT
           END-IF
      *
      *    前回退院日≦入院日となっていること
      *    前歴が自（他）院歴の場合は前回退院日＜入院日となっていること
           MOVE    1               TO  IDXM
           IF    ( SPA-NAI-I01-MAEIDOYMD     NOT =   SPACE )
               MOVE    2           TO  IDXM
           END-IF
      D    DISPLAY IDXM ":TNYUINYMD=" SPA-NAI-I01-TNYUINYMD (IDXM)
      D            "/TTAIINYMD=" SPA-NAI-I01-TTAIINYMD (IDXM)
      D            "/TZITA-FLG=" SPA-NAI-I01-TZITA-FLG (IDXM)
           IF    ( SPA-NAI-I01-TTAIINYMD (IDXM) NOT =   SPACE )
      *
               EVALUATE    SPA-NAI-I01-TZITA-FLG (IDXM)
               WHEN    ZERO
                   IF    ( SPA-NAI-I01-TTAIINYMD (IDXM)
                                  <=    SPA-NAI-I01-NYUINYMD )
                       CONTINUE
                   ELSE
                       MOVE    "1018"      TO  SPA-ERRCD
                       MOVE    7           TO  SPA-GMN-I01-CUR
                       GO  TO  4801-NYUIN-TOUROKU-CHK-EXT
                   END-IF
      *
      *            同日再入院の場合継続入院となっていること
      *-           IF    ( SPA-NAI-I01-TTAIINYMD (IDXM)
      *-                           =    SPA-NAI-I01-NYUINYMD )
      *-               IF    ( SPA-GMN-I01-SHOKAI-CD   =   "2" )
      *-                 AND ( SPA-GMN-I01-SHONUM
      *-                           =   SPA-GMN-I01-TSHONUM (IDXM)  )
      *-                   CONTINUE
      *-               ELSE
      *-                   MOVE    "1038"  TO  SPA-ERRCD
      *-                   MOVE    10      TO  SPA-GMN-I01-CUR
      *-                   GO  TO  4801-NYUIN-TOUROKU-CHK-EXT
      *-               END-IF
      *-           END-IF
               WHEN    1
                   IF    ( SPA-NAI-I01-TTAIINYMD (IDXM)
                                  <=    SPA-NAI-I01-NYUINYMD )
                       IF    ( SPA-NAI-I01-TTAIINYMD (IDXM)
                                   =    SPA-NAI-I01-NYUINYMD )
                           MOVE    SPA-NAI-I01-TNYUINYMD (IDXM)
                                           TO  WRK-STYMD
                           MOVE    SPA-NAI-I01-TTAIINYMD (IDXM)
                                           TO  WRK-EDYMD
                           PERFORM 5002-KIKAN-CALC-SEC
                           IF    ( SPA-NAI-I01-TNYURRKTAISYO (IDXM)
                                           =   WRK-NISSU2 )
                               MOVE    1   TO  FLG-TAINDOUJITU
                           ELSE
                               MOVE    2   TO  FLG-TAINDOUJITU
                           END-IF
                       END-IF
                   ELSE
                       MOVE    "1018"      TO  SPA-ERRCD
                       MOVE    7           TO  SPA-GMN-I01-CUR
                       GO  TO  4801-NYUIN-TOUROKU-CHK-EXT
                   END-IF
               WHEN    OTHER
                   IF    ( SPA-NAI-I01-TTAIINYMD (IDXM)
                                  <     SPA-NAI-I01-NYUINYMD )
                       CONTINUE
                   ELSE
                       MOVE    "1036"      TO  SPA-ERRCD
                       MOVE    7           TO  SPA-GMN-I01-CUR
                       GO  TO  4801-NYUIN-TOUROKU-CHK-EXT
                   END-IF
      *
      *            前回自院歴チェック
                   PERFORM 4801-ZENKAI-ZIINRRK-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  4801-NYUIN-TOUROKU-CHK-EXT
                   END-IF
               END-EVALUATE
           END-IF
      *
           .
       4801-NYUIN-TOUROKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    前回自院歴チェック
      *****************************************************************
       4801-ZENKAI-ZIINRRK-CHK-SEC     SECTION.
      *
      *    前回入院歴（自院分）をチェック
           MOVE    SPACE               TO  WRK-NYUINCHU-TAIINYMD
           MOVE    ZERO                TO  FLG-WKPTNYUINRRK
      *
           INITIALIZE      WKPTNYUIN-REC
           MOVE    SPA-HOSPNUM         TO  WKPTNYUIN-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  WKPTNYUIN-PTID
           MOVE    SPA-NAI-I01-PTRRKNUM TO WKPTNYUIN-RRKNUM
      *
           MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  WRK-YMD
           MOVE    "1"                 TO  WRK-ZOGENPTN
           MOVE    -1                  TO  WRK-ZOGEN
           PERFORM 5002-CALENDAR-SEC
           MOVE    WRK-KEISANYMD       TO  WKPTNYUIN-NYUINYMD
                                           WRK-NYUINCHU-TAIINYMD
      *
           MOVE    WKPTNYUIN-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key76"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-WKPTNYUINRRK
           ELSE
               MOVE    MCPDATA-REC     TO  WKPTNYUIN-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key76"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-WKPTNYUINRRK    NOT =   ZERO )
               GO  TO  4801-ZENKAI-ZIINRRK-CHK-EXT
           END-IF
      *
      *    前歴の自院歴が「入院中」の場合
           IF    ( WKPTNYUIN-JTIKBN            =   "6" )
            AND  ( WKPTNYUIN-NYUINCHUKBN       =   "1" )
      *
      *        継続入院であること
               IF    ( SPA-GMN-I01-SHOKAI-CD   NOT =   "2" )
                   MOVE    "1031"      TO  SPA-ERRCD
                   MOVE    9           TO  SPA-GMN-I01-CUR
                   GO TO   4801-ZENKAI-ZIINRRK-CHK-EXT
               END-IF
      *
      *        初回番号が等しいこと
               IF    ( WKPTNYUIN-SHONUM NOT =   SPA-GMN-I01-SHONUM )
                   MOVE    "1031"      TO  SPA-ERRCD
                   MOVE    10          TO  SPA-GMN-I01-CUR
                   GO TO   4801-ZENKAI-ZIINRRK-CHK-EXT
               END-IF
      *
      *        自院歴の退院日の翌日が入院日となっていること
               IF    ( WKPTNYUIN-TAIINYMD
                                   NOT =   WRK-NYUINCHU-TAIINYMD )
                   MOVE    "1032"      TO  SPA-ERRCD
                   MOVE    7           TO  SPA-GMN-I01-CUR
                   GO TO   4801-ZENKAI-ZIINRRK-CHK-EXT
               END-IF
           END-IF
      *
           .
       4801-ZENKAI-ZIINRRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院登録時関連チェック
      *****************************************************************
       4801-TAIIN-TOUROKU-CHK-SEC      SECTION.
      *
      *    退院日
           IF    ( SPA-GMN-I01-TAIINYMD    =   SPACE )
               MOVE    "1020"      TO  SPA-ERRCD
               MOVE    11          TO  SPA-GMN-I01-CUR
               GO  TO  4801-TAIIN-TOUROKU-CHK-EXT
           END-IF
      *
      *    前回異動日が表示されていない場合は入院日と退院日を比較
      *    前回異動日が表示されている場合は前回異動日と退院日を比較
           IF    ( SPA-GMN-I01-MAEIDOYMD  =   SPACE   )
               IF    ( SPA-NAI-I01-NYUINYMD
                                      <=   SPA-NAI-I01-TAIINYMD )
                   CONTINUE
               ELSE
                   MOVE    "1012"      TO  SPA-ERRCD
                   MOVE    11          TO  SPA-GMN-I01-CUR
                   GO  TO  4801-TAIIN-TOUROKU-CHK-EXT
               END-IF
           ELSE
               IF    ( SPA-NAI-I01-MAEIDOYMD
                                      <=   SPA-NAI-I01-TAIINYMD )
                   CONTINUE
               ELSE
                   MOVE    "1013"      TO  SPA-ERRCD
                   MOVE    11          TO  SPA-GMN-I01-CUR
                   GO  TO  4801-TAIIN-TOUROKU-CHK-EXT
               END-IF
           END-IF
      *
      *    同日再入院の場合、入院日≠退院日となっていること
           IF    ( SPA-NAI-I01-DJKBN   =   "1" OR "2" )
               IF    ( SPA-NAI-I01-NYUINYMD =   SPA-NAI-I01-TAIINYMD )
                   MOVE    "1037"      TO  SPA-ERRCD
                   MOVE    11          TO  SPA-GMN-I01-CUR
                   GO  TO  4801-TAIIN-TOUROKU-CHK-EXT
               END-IF
           END-IF
      *
      *    定期請求との関連チェック
      *    請求開始日取得処理
           PERFORM 4801-SKYSTYMD-GET-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4801-TAIIN-TOUROKU-CHK-EXT
           END-IF
      *
      *    前回定期請求日の翌日（あるいは入院日）
      *    〜算定開始日の翌月までの
      *    日付が入力されていること
           MOVE    WRK-NOW-SKYSTYMD
                                   TO  WRK-YMD
           MOVE    "2"             TO  WRK-ZOGENPTN
           MOVE    1               TO  WRK-ZOGEN
           PERFORM 5002-CALENDAR-SEC
      *
           IF    ( SPA-NAI-I01-TAIINYMD (1 : 6)
                                   <= WRK-KEISANYMD (1 : 6) )
               CONTINUE
           ELSE
               MOVE    "1025"      TO  SPA-ERRCD
               MOVE    11          TO  SPA-GMN-I01-CUR
               GO  TO  4801-TAIIN-TOUROKU-CHK-EXT
           END-IF
      *
      *    月途中の定期請求終了後...(1)、
      *    定期請求区分を「2 月末時のみ請求」に変更された場合、
      *    (1)で作成された収納が未入金であること
           PERFORM 4801-MAE-TEIKI-SKYEDYMD-GET-SEC
           IF    ( SPA-GMN-I01-SEIKYU-CD   =   "2" )
             AND ( WRK-MAE-SKYEDYMD    NOT =   SPACE )
               MOVE    WRK-MAE-SKYEDYMD
                                       TO  WRK-YMD
               MOVE    "1"             TO  WRK-ZOGENPTN
               MOVE    1               TO  WRK-ZOGEN
               PERFORM 5002-CALENDAR-SEC
               IF    ( WRK-KEISANYMD (7 : 2)   NOT =   "01" )
                   MOVE    ZERO                TO  FLG-SYUNOU
                   INITIALIZE  SYUNOU-REC
                   MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
                   MOVE    SPA-NAI-I01-PTID    TO  SYU-PTID
                   MOVE    "1"                 TO  SYU-NYUGAIKBN
                   MOVE    WRK-NOW-SKYSTYMD    TO  SYU-SKYSTYMD
                   MOVE    SPA-NAI-I01-TAIINYMD
                                           TO  SYU-SKYEDYMD
                   MOVE    SYUNOU-REC          TO  MCPDATA-REC
                   MOVE    "tbl_syunou"        TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC          NOT =   ZERO )
                       MOVE    1               TO  FLG-SYUNOU
                   END-IF
      *
                   PERFORM UNTIL   ( FLG-SYUNOU    NOT =   ZERO )
                            OR     ( SPA-ERRCD     NOT =   SPACE )
                       MOVE    MCPDATA-REC     TO  SYUNOU-REC
                       IF    ( SYU-NYUIN-RRKNUM
                                           =   SPA-NAI-I01-PTRRKNUM )
                        AND  ( SYU-DENPJTIKBN
                                       NOT =   "3" )
      *                 AND  ( SYU-CREATEKBN   =   "2" )
                           MOVE    1           TO  FLG-SYUARI
                           IF    ( SYU-NYUKIN-TOTAL    >   ZERO )
                               MOVE    "1028"  TO  SPA-ERRCD
                           END-IF
                       END-IF
      *
                       MOVE    "tbl_syunou"    TO  MCP-TABLE
                       MOVE    "key5"          TO  MCP-PATHNAME
                       PERFORM 910-DBFETCH-SEC
                       IF    ( MCP-RC      NOT =   ZERO )
                           MOVE    1           TO  FLG-SYUNOU
                       END-IF
      *
                   END-PERFORM
      *
                   MOVE    "tbl_syunou"        TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
      *
                   IF    ( SPA-ERRCD     NOT =   SPACE )
                       MOVE    11          TO  SPA-GMN-I01-CUR
                       GO  TO  4801-TAIIN-TOUROKU-CHK-EXT
                   END-IF
               END-IF
      *
           END-IF
      *
      *    退院月に会計テーブルが存在していること
           MOVE    SPA-NAI-I01-TAIINYMD (1 : 6)
                                       TO  WRK-NACCT-SRYYM
           PERFORM 4801-NYUINACCT-SEL-SEC
           IF    ( FLG-NYUINACCTARI    =   ZERO )
               MOVE    "1030"          TO  SPA-ERRCD
               GO  TO  4801-TAIIN-TOUROKU-CHK-EXT
           END-IF
      *
           .
       4801-TAIIN-TOUROKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求開始日取得処理
      *****************************************************************
       4801-SKYSTYMD-GET-SEC           SECTION.
      *
      *    前回定期請求終了日取得処理
           PERFORM 4801-MAE-TEIKI-SKYEDYMD-GET-SEC
           IF    ( WRK-MAE-SKYEDYMD    NOT =   SPACE )
               IF    ( WRK-MAE-SKYEDYMD
                                       >=  SPA-NAI-I01-TAIINYMD )
                   MOVE    "1026"      TO  SPA-ERRCD
                   MOVE    11          TO  SPA-GMN-I01-CUR
                   GO  TO  4801-SKYSTYMD-GET-EXT
               ELSE
                   MOVE    WRK-MAE-SKYEDYMD
                                       TO  WRK-YMD
                   MOVE    "1"         TO  WRK-ZOGENPTN
                   MOVE    1           TO  WRK-ZOGEN
                   PERFORM 5002-CALENDAR-SEC
                   MOVE    WRK-KEISANYMD
                                       TO  WRK-NOW-SKYSTYMD
                   IF    ( SPA-GMN-I01-SEIKYU-CD   =   "2" )
                       IF   ( SPA-NAI-I01-NYUINYMD (1 : 6)
                           <  WRK-NOW-SKYSTYMD     (1 : 6) )
                           MOVE    "01"    TO  WRK-NOW-SKYSTYMD (7 : 2)
                       ELSE
                           MOVE    SPA-NAI-I01-NYUINYMD
                                           TO  WRK-NOW-SKYSTYMD
                           IF    ( WRK-NOW-SKYSTYMD  <   CONST-H180401 )
                               IF    ( SPA-NAI-I01-DJKBN =   "1" )
                                   MOVE    SPA-NAI-I01-NYUINYMD
                                                   TO  WRK-YMD
                                   MOVE    "1"     TO  WRK-ZOGENPTN
                                   MOVE    1       TO  WRK-ZOGEN
                                   PERFORM 5002-CALENDAR-SEC
                                   MOVE    WRK-KEISANYMD
                                           TO  WRK-NOW-SKYSTYMD
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           ELSE
               MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  WRK-NOW-SKYSTYMD
               IF    ( WRK-NOW-SKYSTYMD  <   CONST-H180401 )
                   IF    ( SPA-NAI-I01-DJKBN   =   "1" )
                       MOVE    SPA-NAI-I01-NYUINYMD
                                           TO  WRK-YMD
                       MOVE    "1"         TO  WRK-ZOGENPTN
                       MOVE    1           TO  WRK-ZOGEN
                       PERFORM 5002-CALENDAR-SEC
                       MOVE    WRK-KEISANYMD
                                           TO  WRK-NOW-SKYSTYMD
                   END-IF
               END-IF
           END-IF
      *
           .
       4801-SKYSTYMD-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更時関連チェック
      *****************************************************************
       4801-HENKOU-CHK-SEC             SECTION.
      *
      *
           .
       4801-HENKOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    照会時関連チェック
      *****************************************************************
       4801-SYOKAI-CHK-SEC             SECTION.
      *
      *
           .
       4801-SYOKAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院取消時関連チェック
      *****************************************************************
       4801-NYUIN-TORIKESI-CHK-SEC     SECTION.
      *
      *
           .
       4801-NYUIN-TORIKESI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院取消時関連チェック
      *****************************************************************
       4801-TAIIN-TORIKESI-CHK-SEC     SECTION.
      *
        
      *
           .
       4801-TAIIN-TORIKESI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    転科　転棟　転室時関連チェック
      *****************************************************************
       4801-TEKNA-CHK-SEC              SECTION.
      *
      *    異動日が入力されていること
           IF    ( SPA-GMN-I01-IDOYMD  =   SPACE )
               MOVE    "1014"      TO  SPA-ERRCD
               MOVE    3           TO  SPA-GMN-I01-CUR
               GO  TO  4801-TEKNA-CHK-EXT
           END-IF
      *
      *    前回異動日が表示されていない場合は入院日と異動日を比較
      *    前回異動日が表示されている場合は前回異動日と異動日を比較
           IF    ( SPA-GMN-I01-MAEIDOYMD  =   SPACE   )
               IF    ( SPA-NAI-I01-NYUINYMD
                                      <=   SPA-NAI-I01-IDOYMD )
                   CONTINUE
               ELSE
                   MOVE    "1015"      TO  SPA-ERRCD
                   MOVE    3           TO  SPA-GMN-I01-CUR
                   GO  TO  4801-TEKNA-CHK-EXT
               END-IF
           ELSE
               IF    ( SPA-NAI-I01-MAEIDOYMD
                                      <=   SPA-NAI-I01-IDOYMD )
                   CONTINUE
               ELSE
                   MOVE    "1016"      TO  SPA-ERRCD
                   MOVE    3           TO  SPA-GMN-I01-CUR
                   GO  TO  4801-TEKNA-CHK-EXT
               END-IF
           END-IF
      *
      *    *** 2004.03.11 削除 ***
      *    入院科または、病室番号または、病棟名または、特定入院料のいずれ
      *    かが変更されていること 
      *-   IF    ( SPA-GMN-I01-NYUINKA-CD  =   SPA-NAI-I01-NYUINKA-MOT )
      *-    AND  ( SPA-GMN-I01-BTUNAME     =   SPA-NAI-I01-BTUNAME-MOT )
      *-    AND  ( SPA-GMN-I01-BRMNUM      =   SPA-NAI-I01-BRMNUM-MOT  )
      *-    AND  ( SPA-GMN-I01-HKNCOMBI-CD =   SPA-NAI-I01-HKNCOMBI-MOT)
      *-    AND  ( SPA-NAI-I01-TOKNYUIN-ORG
      *-                                   =   SPA-NAI-I01-TOKNYUINMOT )
      *-           MOVE    "1017"      TO  SPA-ERRCD
      *-           MOVE    4           TO  SPA-GMN-I01-CUR
      *-           GO  TO  4801-TEKNA-CHK-EXT
      *-   END-IF
      *
           IF    ( SPA-NAI-I01-IDOYMD (1:6)
                                       >   SPA-NAI-I01-NYUINYMD (1:6))
               MOVE    SPA-NAI-I01-IDOYMD
                                       TO  WRK-YMD
               MOVE    "2"             TO  WRK-ZOGENPTN
               MOVE    -1              TO  WRK-ZOGEN
               PERFORM 5002-CALENDAR-SEC
               MOVE    WRK-KEISANYMD   TO  WRK-NACCT-SRYYM
               PERFORM 4801-NYUINACCT-SEL-SEC
               IF    ( FLG-NYUINACCTARI    =   ZERO )
                   MOVE    "1042"      TO  SPA-ERRCD
                   GO  TO  4801-TEKNA-CHK-EXT
               END-IF
      *
           END-IF
      *
           IF    ( SPA-NAI-I01-IDOYMD (7:2)  NOT =   "01" )
               MOVE    SPA-NAI-I01-IDOYMD (1 : 6)
                                       TO  WRK-NACCT-SRYYM
               PERFORM 4801-NYUINACCT-SEL-SEC
               IF    ( FLG-NYUINACCTARI    =   ZERO )
                   MOVE    "1029"          TO  SPA-ERRCD
                   GO  TO  4801-TEKNA-CHK-EXT
               END-IF
           END-IF
      *
           .
       4801-TEKNA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       4801-NYUINACCT-SEL-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCTARI
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM          TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SPA-NAI-I01-PTID    TO  NACCT-PTID
           MOVE    WRK-NACCT-SRYYM
                                       TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM UNTIL ( FLG-NYUINACCT       NOT =   ZERO )
                    OR   ( FLG-NYUINACCTARI    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
      *
               IF    ( NACCT-ZAISKBKBN     =    "1"    )
                   MOVE    1           TO  FLG-NYUINACCTARI
               END-IF
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE     1          TO  FLG-NYUINACCT
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4801-NYUINACCT-SEL-EXT.
           EXIT.
      *****************************************************************
      *    異動取消時関連チェック
      *****************************************************************
       4801-IDO-TORIKESI-CHK-SEC       SECTION.
      *
        
      *
           .
       4801-IDO-TORIKESI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院再計算関連チェック
      *****************************************************************
       4801-TAIIN-SAIKEISAN-CHK-SEC       SECTION.
      *
      *    歴が選択されていること
           IF    ( SPA-GMN-I01-NYUREKILST-SEL >   ZERO )
            AND  ( SPA-GMN-I01-NYUREKILST-SEL
                              <=   SPA-GMN-I01-NYUREKILST-CNT )
               CONTINUE
           ELSE
               MOVE    "1034"          TO  SPA-ERRCD
               GO  TO  4801-TAIIN-SAIKEISAN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-NYUREKILST-SEL
                                       TO  IDX1
      *
      *    歴作成データが選択されていないこと
           IF    ( SPA-NAI-I01-TZITA-FLG (IDX1)    NOT =   ZERO )
               MOVE    "1035"          TO  SPA-ERRCD
               GO  TO  4801-TAIIN-SAIKEISAN-CHK-EXT
           END-IF
      *
      *    入院中の歴が選択されていないこと
           IF    ( SPA-NAI-I01-TTAIINYMD (IDX1)        =   SPACE )
               MOVE    "1034"          TO  SPA-ERRCD
               GO  TO  4801-TAIIN-SAIKEISAN-CHK-EXT
           END-IF
      *
           .
       4801-TAIIN-SAIKEISAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴データ編集
      *****************************************************************
       4801-PTNYUINRRK-EDIT-SEC    SECTION.
      *
      *    入院履歴データ編集前処理
           PERFORM 4801-PTNYUINRRK-EDIT-MAE-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4801-PTNYUINRRK-EDIT-EXT
           END-IF
      *
           INITIALIZE              PTNYUINRRK-REC
      *
      *    医療機関ＩＤ
           MOVE    SPA-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
      *
      *    患者ＩＤ
           MOVE    SPA-NAI-I01-PTID
                                   TO  PTNYUINRRK-PTID
      *
      *    入院履歴番号（入院１回で＋１ずつ採番）
           MOVE    WRK-RRKNUM      TO  PTNYUINRRK-RRKNUM
      *
      *    入院履歴枝番号（転科・転室・転棟時に採番）
           MOVE    WRK-RRKEDANUM   TO  PTNYUINRRK-RRKEDANUM
      *
      *    病棟番号
           MOVE    SPA-GMN-I01-BTUNAME-CD
                                   TO  PTNYUINRRK-BTUNUM
           MOVE    SPA-GMN-I01-BTUNAME-CD
                                   TO  PTNYUINRRK-BRMNUM (1 : 2)
      *
      *    病棟名称
      *    MOVE    SPA-GMN-I01-BTUNAME-NM
      *                            TO  PTNYUINRRK-BTUNAME
      *
      *    病室番号
           MOVE    SPA-GMN-I01-BRMNUM
                                   TO  WRK-BRMNUM
      *    病室番号が数字の場合、前ゼロ編集を行う
           PERFORM 4800-BRMNUM-EDIT-SEC
           MOVE    WRK-BRMNUM-EDT  TO  PTNYUINRRK-BRMNUM (3 : 6)
      *    入院科
           MOVE    SPA-GMN-I01-NYUINKA-CD
                                   TO  PTNYUINRRK-NYUINKA
      *    保険組合せ番号
           MOVE    SPA-GMN-I01-HKNCOMBI-CD
                                   TO  PTNYUINRRK-HKNCOMBINUM
      *    入院日
           MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  PTNYUINRRK-NYUINYMD
      *    退院日、退院事由
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
           WHEN    "01"
           WHEN    "07"
               MOVE    "99999999"  TO  PTNYUINRRK-TAIINYMD
               MOVE    SPACE       TO  PTNYUINRRK-TAIINCD
           WHEN    "02"
           WHEN    "11"
               MOVE    SPA-NAI-I01-TAIINYMD
                                   TO  PTNYUINRRK-TAIINYMD
               MOVE    SPA-GMN-I01-TAIINCD
                                   TO  PTNYUINRRK-TAIINCD
           WHEN    OTHER
               MOVE    BAKRRK-TAIINYMD
                                   TO  PTNYUINRRK-TAIINYMD
               MOVE    BAKRRK-TAIINCD
                                   TO  PTNYUINRRK-TAIINCD
           END-EVALUATE
      *
      *    特定入院料区分
           MOVE    SPA-NAI-I01-TOKNYUIN-ORG-KBN
                                   TO  PTNYUINRRK-TOKUTEI-KBN
      *    特定入院料
           MOVE    SPA-NAI-I01-TOKNYUIN-ORG-CD
                                   TO  PTNYUINRRK-TOKUTEI-NYUIN
      *    特定入院料非該当
           IF    ( SPA-NAI-I01-TOKHIGAITOCD    NOT =   SPACE )
            AND  ( SPA-GMN-I01-TOKHIGAITO-CD   NOT =   "0"  )
               MOVE    SPA-GMN-I01-TOKHIGAITO-CD
                                   TO  PTNYUINRRK-TOKU-SANTEIYOUKENKBN
           ELSE
               MOVE    SPACE       TO  PTNYUINRRK-TOKU-SANTEIYOUKENKBN
           END-IF
      *
      *    室料差額
           MOVE    SPA-GMN-I01-SAGAKU-CD
                                   TO  PTNYUINRRK-RM-SAGAKU
      *    定期請求区分
           MOVE    SPA-GMN-I01-SEIKYU-CD
                                   TO  PTNYUINRRK-TEIKI-SEIKYUKBN
      *    検索表示区分
           MOVE    SPA-GMN-I01-DISPKBN-CD
                                   TO  PTNYUINRRK-KENSAKU-DISPKBN
      *    初回継続区分
           MOVE    SPA-GMN-I01-SHOKAI-CD
                                   TO  PTNYUINRRK-SHOKAIKBN
      *    初回番号
           MOVE    WRK-SHONUM
                                   TO  PTNYUINRRK-SHONUM
      *    介護異動区分
           MOVE    SPA-GMN-I01-KAIGO-CD
                                   TO  PTNYUINRRK-KAIGOKBN
      *
      *    担当医１
           IF    ( SPA-GMN-I01-DR-CD (1)   NOT =   SPACE )
               MOVE    "1"         TO  PTNYUINRRK-DRCD1 ( 1 : 1 ) 
               MOVE    SPA-GMN-I01-DR-CD (1)
                                   TO  PTNYUINRRK-DRCD1 ( 2 : )
           END-IF
      *    担当医２
           IF    ( SPA-GMN-I01-DR-CD (2)   NOT =   SPACE )
               MOVE    "1"         TO  PTNYUINRRK-DRCD2 ( 1 : 1 ) 
               MOVE    SPA-GMN-I01-DR-CD (2)
                                   TO  PTNYUINRRK-DRCD2 ( 2 : )
           END-IF
      *    担当医３
           IF    ( SPA-GMN-I01-DR-CD (3)   NOT =   SPACE )
               MOVE    "1"         TO  PTNYUINRRK-DRCD3 ( 1 : 1 ) 
               MOVE    SPA-GMN-I01-DR-CD (3)
                                   TO  PTNYUINRRK-DRCD3 ( 2 : )
           END-IF
      *    状態区分
      *    （１：入院　２：転科　３：転棟　４：転室　５：他院歴
      *      ６：自院歴 ）
      *    転入日
      *    転出日
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
      *    入院登録
           WHEN    "01"
               MOVE    "1"         TO  PTNYUINRRK-JTIKBN
               MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  PTNYUINRRK-TENNYUYMD
               MOVE    "99999999"  TO  PTNYUINRRK-TENSTUYMD
      *    退院登録
      *    WHEN    "02"
      *        MOVE    BAKRRK-JTIKBN  TO  PTNYUINRRK-JTIKBN
      *        MOVE    BAKRRK-TENNYUYMD
      *                            TO  PTNYUINRRK-TENNYUYMD
      *        MOVE    SPA-NAI-I01-TAIINYMD
      *                            TO  PTNYUINRRK-TENSTUYMD
      *    退院取消
           WHEN    "07"
               MOVE    BAKRRK-JTIKBN  TO  PTNYUINRRK-JTIKBN
               MOVE    BAKRRK-TENNYUYMD
                                   TO  PTNYUINRRK-TENNYUYMD
               MOVE    "99999999"  TO  PTNYUINRRK-TENSTUYMD
      *    転科　転棟　転室
           WHEN    "08"
      *
               EVALUATE    TRUE
      *        科が変わった場合
               WHEN    WRK-NYUINKA-KEY NOT =   PTNYUINRRK-NYUINKA
                   MOVE    "2"   TO  PTNYUINRRK-JTIKBN
      *        病棟が変わった場合
               WHEN    WRK-BTUNUM-KEY  NOT =   PTNYUINRRK-BTUNUM
                   MOVE    "3"   TO  PTNYUINRRK-JTIKBN
      *        病室が変わった場合
               WHEN    WRK-BRMNUM-KEY  NOT =   PTNYUINRRK-BRMNUM
                   MOVE    "4"   TO  PTNYUINRRK-JTIKBN
      *        入院料が変わったとき
               WHEN  ( WRK-HKNCOMBI-KEY
                                       NOT  =  PTNYUINRRK-HKNCOMBINUM )
               WHEN  ( WRK-TOKUTEI-KBN-KEY
                                   NOT =   PTNYUINRRK-TOKUTEI-KBN )
                 OR  ( WRK-TOKUTEI-NYUIN-KEY
                                   NOT =   PTNYUINRRK-TOKUTEI-NYUIN )
                   MOVE    "7"   TO  PTNYUINRRK-JTIKBN
               WHEN    OTHER
                   MOVE    "7"   TO  PTNYUINRRK-JTIKBN
               END-EVALUATE
      *
               MOVE    SPA-NAI-I01-IDOYMD
                                   TO  PTNYUINRRK-TENNYUYMD
               MOVE    "99999999"  TO  PTNYUINRRK-TENSTUYMD
      *    その他
           WHEN    OTHER
               MOVE    BAKRRK-JTIKBN  TO  PTNYUINRRK-JTIKBN
               MOVE    BAKRRK-TENNYUYMD
                                   TO  PTNYUINRRK-TENNYUYMD
               MOVE    BAKRRK-TENSTUYMD
                                   TO  PTNYUINRRK-TENSTUYMD
           END-EVALUATE
      *
      *    入院中区分 ("0"固定)
           MOVE    "0"             TO  PTNYUINRRK-NYUINCHUKBN
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" )
               MOVE    1           TO  IDXM
               IF    ( SPA-NAI-I01-MAEIDOYMD   NOT =   SPACE )
                   MOVE    2       TO  IDXM
               END-IF
      D    DISPLAY IDXM ":TNYUINYMD=" SPA-NAI-I01-TNYUINYMD (IDXM)
      D            "/TTAIINYMD=" SPA-NAI-I01-TTAIINYMD (IDXM)
      D            "/TZITA-FLG=" SPA-NAI-I01-TZITA-FLG (IDXM)
               IF    ( SPA-NAI-I01-TTAIINYMD (IDXM)    NOT =   SPACE )
                   IF    ( SPA-NAI-I01-TTAIINYMD (IDXM)
                                   =   SPA-NAI-I01-NYUINYMD )
                       EVALUATE    SPA-NAI-I01-TZITA-FLG (IDXM)
                       WHEN    ZERO
                           MOVE    "1" TO  PTNYUINRRK-DOUJITSUKBN
                       WHEN    1
                           MOVE    "2" TO  PTNYUINRRK-DOUJITSUKBN
                       END-EVALUATE
                   END-IF
               END-IF
           ELSE
                       MOVE    SPA-NAI-I01-DJKBN
                                   TO  PTNYUINRRK-DOUJITSUKBN
           END-IF
      *
      *    食事区分
           MOVE    SPA-GMN-I01-SKJKBN-CD
                                   TO  PTNYUINRRK-SKJKBN
      *
           EVALUATE    SPA-GMN-I01-SKJKBN-CD
           WHEN    "B"
               MOVE    SPA-NAI-I01-TMAESKJKBN-ASA
                                   TO  PTNYUINRRK-SKJKBN-ASA
               MOVE    SPA-NAI-I01-TMAESKJKBN-HIRU
                                   TO  PTNYUINRRK-SKJKBN-HIRU
               MOVE    SPA-NAI-I01-TMAESKJKBN-YU
                                   TO  PTNYUINRRK-SKJKBN-YU
           WHEN    "C"
               MOVE    SPA-GMN-I01-TSKJKBN-ASA-CD
                                   TO  PTNYUINRRK-SKJKBN-ASA
               MOVE    SPA-GMN-I01-TSKJKBN-HIRU-CD
                                   TO  PTNYUINRRK-SKJKBN-HIRU
               MOVE    SPA-GMN-I01-TSKJKBN-YU-CD
                                   TO  PTNYUINRRK-SKJKBN-YU
           WHEN    OTHER
               MOVE    SPACE       TO  PTNYUINRRK-SKJKBN-ASA
                                       PTNYUINRRK-SKJKBN-HIRU
                                       PTNYUINRRK-SKJKBN-YU
           END-EVALUATE
      *
      *    食事時間区分
           MOVE    SPA-GMN-I01-TIMEKBN-CD
                                   TO  PTNYUINRRK-TIMEKBN-IN
      *
      *    選定入院区分
           MOVE    SPA-GMN-I01-SENTEIKBN-CD
                                   TO  PTNYUINRRK-SENTEIKBN
      *    入院料算定区分
           MOVE    SPA-GMN-I01-NYUCALKBN-CD
                                   TO  PTNYUINRRK-NYUCALKBN
      *
      *    入院加算
      D     DISPLAY "UPDKBN=" WRK-UPDKBN
      D             " " PTNYUINRRK-KSNSRYCD (1)
      D             " " PTNYUINRRK-KSNSRYCD (2)
      D             " " PTNYUINRRK-KSNSRYCD (3)
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "01" OR "08" )
               MOVE    ZERO        TO  IDXK
               PERFORM VARYING IDX0    FROM   1   BY  1
                   UNTIL ( IDX0 >   CONST-GMN-KSN-MAX )
                   IF    ( SPA-GMN-I01-KSN-CD (IDX0)     =   "2" )
                        COMPUTE IDXK   =   IDXK   +   1
                        MOVE    SPA-GMN-I01-KSN-SRYCD (IDX0)
                                   TO  PTNYUINRRK-KSNSRYCD (IDXK)
                   END-IF
               END-PERFORM
           ELSE
               MOVE    BAKRRK-KSNSRYCD-G
                                   TO  PTNYUINRRK-KSNSRYCD-G
           END-IF
      *
           IF    ( WRK-UPDKBN      =   "UPD" )
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    BAKRRK-CREYMD   TO  PTNYUINRRK-CREYMD
               MOVE    SMCNDATE-YMD    TO  PTNYUINRRK-UPYMD
               MOVE    SMCNDATE-HMS    TO  PTNYUINRRK-UPHMS
           ELSE
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD    TO  PTNYUINRRK-CREYMD
                                           PTNYUINRRK-UPYMD
               MOVE    SMCNDATE-HMS    TO  PTNYUINRRK-UPHMS
           END-IF
      *
           .
      *
       4801-PTNYUINRRK-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴データ編集
      *****************************************************************
       4801-PTNYUINRRK-EDIT-MAE-SEC    SECTION.
      *
           INITIALIZE                  BAKRRK-REC
      *
           MOVE    SPACE           TO  WRK-NYUINKA-KEY
                                       WRK-BTUNUM-KEY
                                       WRK-BRMNUM-KEY
                                       WRK-TOKUTEI-KEY
           MOVE    ZERO            TO  WRK-SHONUM
      *
           PERFORM 900-PTNYUINRRK-KEY-SEL-SEC
           MOVE    PTNYUINRRK-REC  TO  BAKRRK-REC
      *
      D    DISPLAY "PTRRKNUM=" SPA-NAI-I01-PTRRKNUM
      *
      *    入院登録時、他院退院日との同一入院時で他院歴の
      *    通算日数を調整する場合
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" )
            AND  ( FLG-TAINTUSANMINUS      =   1 )
               PERFORM 4801-TAINTUSANMINUS-UPD-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  4801-PTNYUINRRK-EDIT-MAE-EXT
               END-IF
           END-IF
      *
           MOVE    SPA-NAI-I01-PTRRKNUM
                                   TO  WRK-RRKNUM
           MOVE    SPA-NAI-I01-PTRRKEDANUM
                                   TO  WRK-RRKEDANUM
      *
      *    入院登録で初回の場合、設定する初回番号を求める
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" )
            AND  ( SPA-GMN-I01-SHOKAI-CD   =   "1"  )
               PERFORM 4801-SYOKAI-GET-SEC
           ELSE
               COMPUTE WRK-SHONUM      =   SPA-GMN-I01-SHONUM
           END-IF
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "01" )
            AND  ( SPA-NAI-I01-MAEIDOYMD   NOT =   SPACE )
            AND  ( BAKRRK-TIMEKBN-IN           =   "1" OR "2" OR "3" )
               PERFORM 4801-NYUINSKJ-UPDATE-SEC
           END-IF
      *
      *    退院取消処理時、履歴を遡って退院日を更新する
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "07" )
      *
               PERFORM 4801-TAIINYMD-UPD-SEC
      *
      *        前回定期請求終了日取得処理
               PERFORM 4801-MAE-TEIKI-SKYEDYMD-GET-SEC
               IF    ( WRK-MAE-SKYEDYMD    =   SPACE )
                   MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  WRK-SKYSTYMD
               ELSE
                   MOVE    WRK-MAE-SKYEDYMD
                                       TO  WRK-YMD
                   MOVE    "1"         TO  WRK-ZOGENPTN
                   MOVE    1           TO  WRK-ZOGEN
                   PERFORM 5002-CALENDAR-SEC
                   MOVE    WRK-KEISANYMD
                                       TO  WRK-SKYSTYMD
               END-IF
               MOVE    SPA-NAI-I01-TAIINYMD
                                       TO  WRK-SKYEDYMD
      *        収納データを更新する
               PERFORM 4801-SYUNOU-UPD-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  4801-PTNYUINRRK-EDIT-MAE-EXT
               END-IF
      *
           END-IF
      *
      *    入院取消時、転科歴をワークに退避する
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "05"  OR  "06" )
               PERFORM 4801-TENKAREKI-TAIHI-SEC
      *
      *        -- 定期請求で作成された収納データを取消
      *        前回定期請求終了日取得処理
               PERFORM 4801-MAE-TEIKI-SKYEDYMD-GET-SEC
               IF    ( WRK-MAE-SKYEDYMD    =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  WRK-SKYSTYMD
                   MOVE    WRK-MAE-SKYEDYMD
                                       TO  WRK-SKYEDYMD
      *
      *            患者定期請求履歴を更新する
                   PERFORM 4801-PTTEIKIRRK-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  4801-PTNYUINRRK-EDIT-MAE-EXT
                   END-IF
      *
      *            収納データを更新する
                   PERFORM 4801-SYUNOU-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  4801-PTNYUINRRK-EDIT-MAE-EXT
                   END-IF
      *
               END-IF
           END-IF
      *
      *    転科・転棟・転室の場合、転出元の歴の転出日に異動日を設定
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "08" )
               PERFORM 4801-TENSYUTU-MOTO-UPD-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  4801-PTNYUINRRK-EDIT-MAE-EXT
               END-IF
           END-IF
      *
      *    異動取消の場合、異動前の歴の転出日にALL9を設定
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "09" )
               PERFORM 4801-IDO-MAE-UPD-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  4801-PTNYUINRRK-EDIT-MAE-EXT
               END-IF
           END-IF
      *
           PERFORM 4801-RRKNUM-EDIT-SEC
      *
      *    入院履歴マスタ検索
           PERFORM 4801-PTNYUINRRK-SEL-SEC
           IF    ( FLG-PTNYUINRRK  =   ZERO )
      *        追加処理時、既にデータが存在する場合はエラーとする
               IF    ( WRK-UPDKBN  =   "INS" )
      *            入院歴を再編集
                   PERFORM 4101-NYUREKI-EDIT-SEC
                   MOVE    "4002"  TO  SPA-ERRCD
                   GO  TO  4801-PTNYUINRRK-EDIT-MAE-EXT
               END-IF
           ELSE
      *        更新・削除処理時、対象データが存在しなければエラーとする
               IF    ( WRK-UPDKBN  =   "UPD" OR "DEL")
      *            入院歴を再編集
                   PERFORM 4101-NYUREKI-EDIT-SEC
                   MOVE    "4003"  TO  SPA-ERRCD
                   GO  TO  4801-PTNYUINRRK-EDIT-MAE-EXT
               END-IF
      *
           END-IF
      *
      *
           .
       4801-PTNYUINRRK-EDIT-MAE-EXT.
           EXIT.
      *****************************************************************
      *    初回番号取得処理
      *****************************************************************
       4801-SYOKAI-GET-SEC         SECTION.
      *
      *    入院履歴マスタ検索
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
               COMPUTE WRK-SHONUM  =   1
           ELSE
               IF  ( SPA-NAI-I01-PTRRKNUM
                                   =   PTNYUINRRK-RRKNUM )
                   COMPUTE WRK-SHONUM
                                   =   PTNYUINRRK-SHONUM
               ELSE
                   COMPUTE WRK-SHONUM
                                   =   PTNYUINRRK-SHONUM   +   1
               END-IF
           END-IF
      *
           .
       4801-SYOKAI-GET-EXT.
           EXIT.
      *****************************************************************
      *    入院日食事更新処理
      *****************************************************************
       4801-NYUINSKJ-UPDATE-SEC        SECTION.
      *
           MOVE    BAKRRK-NYUINYMD     TO  WRK-SYMD
      *
           PERFORM 900-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               IF    ( NACCT-DAY (WRK-SDD)     >   ZERO )
                   EVALUATE    NACCT-SRYCDTOTAL
      *            朝
                   WHEN    099999918
                       EVALUATE    TRUE
                       WHEN  ( PTNYUINRRK-TIMEKBN-IN   =   "1" )
                        AND  ( SPA-GMN-I01-TIMEKBN-CD  =   "0" )
                       WHEN  ( PTNYUINRRK-TIMEKBN-IN   =   "1" )
                        AND  ( SPA-GMN-I01-TIMEKBN-CD  =   "2" )
                       WHEN  ( PTNYUINRRK-TIMEKBN-IN   =   "1" )
                        AND  ( SPA-GMN-I01-TIMEKBN-CD  =   "3" )
                           MOVE    ZERO    TO  NACCT-DAY (WRK-SDD)
                       END-EVALUATE
      *            昼
                   WHEN    099999919
                       EVALUATE    TRUE
                       WHEN  ( PTNYUINRRK-TIMEKBN-IN   =   "1" )
                        AND  ( SPA-GMN-I01-TIMEKBN-CD  =   "0" )
                       WHEN  ( PTNYUINRRK-TIMEKBN-IN   =   "1" )
                        AND  ( SPA-GMN-I01-TIMEKBN-CD  =   "3" )
                       WHEN  ( PTNYUINRRK-TIMEKBN-IN   =   "2" )
                        AND  ( SPA-GMN-I01-TIMEKBN-CD  =   "0" )
                       WHEN  ( PTNYUINRRK-TIMEKBN-IN   =   "2" )
                        AND  ( SPA-GMN-I01-TIMEKBN-CD  =   "3" )
                           MOVE    ZERO    TO  NACCT-DAY (WRK-SDD)
                       END-EVALUATE
      *            夕
                   WHEN    099999920
                       EVALUATE    TRUE
                       WHEN  ( PTNYUINRRK-TIMEKBN-IN   =   "1" )
                        AND  ( SPA-GMN-I01-TIMEKBN-CD  =   "0" )
                       WHEN  ( PTNYUINRRK-TIMEKBN-IN   =   "2" )
                        AND  ( SPA-GMN-I01-TIMEKBN-CD  =   "0" )
                       WHEN  ( PTNYUINRRK-TIMEKBN-IN   =   "3" )
                        AND  ( SPA-GMN-I01-TIMEKBN-CD  =   "0" )
                           MOVE    ZERO    TO  NACCT-DAY (WRK-SDD)
                       END-EVALUATE
                   END-EVALUATE
                   MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                   MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                   MOVE     "key"              TO  MCP-PATHNAME
                   PERFORM  910-DBUPDATE-SEC
               END-IF
      *
               PERFORM 900-NYUINACCT-KEY17-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4801-NYUINSKJ-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    退院日更新処理
      *****************************************************************
       4801-TAIINYMD-UPD-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-TAIINUPDCNT
      *
      *    入院履歴マスタ検索
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-I01-PTRRKNUM
                                       TO  PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
      *    入院歴の退院日を更新する。
           PERFORM UNTIL ( MCP-RC      NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
               COMPUTE WRK-TAIINUPDCNT
                   =   WRK-TAIINUPDCNT +   1
      *
      *        最新歴は別途更新処理を行う
               IF    ( WRK-TAIINUPDCNT =   1 )
                   IF    ( SPA-NAI-I01-PTRRKNUM
                                       =   PTNYUINRRK-RRKNUM )
                    AND  ( SPA-NAI-I01-PTRRKEDANUM
                                       =   PTNYUINRRK-RRKEDANUM )
                       CONTINUE
                   ELSE
                       MOVE    "4004"  TO  SPA-ERRCD
                       GO  TO  4801-TAIINYMD-UPD-EXT
                   END-IF
               ELSE
                  MOVE    "99999999"
                                       TO  PTNYUINRRK-TAIINYMD
                  MOVE    SPACE        TO  PTNYUINRRK-TAIINCD
                  PERFORM 800-MCNDATE-SEC
                  MOVE    SMCNDATE-YMD        TO  PTNYUINRRK-UPYMD
                  MOVE    SMCNDATE-HMS        TO  PTNYUINRRK-UPHMS
      *
                   MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4005"  TO  SPA-ERRCD
                       GO  TO  4801-TAIINYMD-UPD-EXT
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( WRK-TAIINUPDCNT     =   ZERO )
               MOVE    "4006"  TO  SPA-ERRCD
               GO  TO  4801-TAIINYMD-UPD-EXT
           END-IF
      *
      *
           .
       4801-TAIINYMD-UPD-EXT.
           EXIT.
      *****************************************************************
      *    収納データ更新処理
      *****************************************************************
       4801-SYUNOU-UPD-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE  SYUNOU-REC
      *
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  SYU-PTID
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    WRK-SKYSTYMD        TO  SYU-SKYSTYMD
           MOVE    WRK-SKYEDYMD        TO  SYU-SKYEDYMD
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
      *
               IF    ( SYU-NYUIN-RRKNUM    =   SPA-NAI-I01-PTRRKNUM )
                AND  ( SYU-DENPJTIKBN  NOT =   "3" )
      *
                   COMPUTE WRK-SKYMONEY
                       =   SYU-SKYMONEY    *   -1
      *
      *            請求取消として収納データを更新
                   MOVE    ZERO            TO  SYU-SKYMONEY
                   MOVE    "3"             TO  SYU-DENPJTIKBN
      *
      *            収納明作成処理
                   PERFORM 4801-SYUMEI-SAKUSEI-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  4801-SYUNOU-UPD-EXT
                   END-IF
      *
                   PERFORM 800-MCNDATE-SEC
                   MOVE    SMCNDATE-YMD    TO  SYU-UPYMD
                   MOVE    SMCNDATE-HMS    TO  SYU-UPHMS
      *
                   MOVE    SYUNOU-REC      TO  MCPDATA-REC
                   MOVE    "tbl_syunou"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4007"      TO  SPA-ERRCD
                       GO  TO  4801-SYUNOU-UPD-EXT
                   END-IF
               END-IF
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    1           TO  FLG-SYUNOU
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       4801-SYUNOU-UPD-EXT.
           EXIT.
      *****************************************************************
      *    前回定期請求終了日取得処理
      *****************************************************************
       4801-MAE-TEIKI-SKYEDYMD-GET-SEC SECTION.
      *
           MOVE    SPACE               TO  WRK-MAE-SKYEDYMD
      *
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTTEIKIRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTTEIKIRRK-PTID
           MOVE    SPA-NAI-I01-PTRRKNUM
                                       TO  PTTEIKIRRK-RRKNUM
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
               MOVE    PTTEIKIRRK-SKYEDYMD
                                       TO  WRK-MAE-SKYEDYMD
           END-IF
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       4801-MAE-TEIKI-SKYEDYMD-GET-EXT.
           EXIT.
      *****************************************************************
      *    収納明細作成処理
      *****************************************************************
       4801-SYUMEI-SAKUSEI-SEC         SECTION.
      *
           INITIALIZE                  SS002-AREA
           CALL    "ORCSS002"      USING   SS002-AREA
                                           SYUNOU-REC
                                           SPA-AREA
      *
           PERFORM 900-SYUMEI-KEY14-SEL-SEC
      *
           COMPUTE IDX13 =  SMEI-DENPEDANUM + 1
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *伝票番号枝番
           MOVE    IDX13               TO  SMEI-DENPEDANUM
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
           MOVE    SPA-SYSYMD          TO  SMEI-NYUHEN-YMD
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
               MOVE    "4008"          TO  SPA-ERRCD
               GO  TO  4801-SYUMEI-SAKUSEI-EXT
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           MOVE    SS002-SYURRKNUM     TO  SMEI-SYURRKNUM
           MOVE    SS002-SYUEDANUM     TO  SMEI-SYUEDANUM
           MOVE    SS002-SYURRKUPDKBN  TO  SMEI-SYURRKUPDKBN
      *
      *    収納明細追加
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4009"          TO  SPA-ERRCD
           END-IF
      *
           .
       4801-SYUMEI-SAKUSEI-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴更新処理
      *****************************************************************
       4801-PTTEIKIRRK-UPD-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
      *
      *    患者請求履歴更新
           INITIALIZE  PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTTEIKIRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTTEIKIRRK-PTID
           MOVE    WRK-SKYSTYMD        TO  PTTEIKIRRK-SKYSTYMD
           MOVE    WRK-SKYEDYMD        TO  PTTEIKIRRK-SKYEDYMD
           MOVE    SPA-NAI-I01-PTRRKNUM
                                       TO  PTTEIKIRRK-RRKNUM
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  PTTEIKIRRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTTEIKIRRK-UPHMS
      *
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "upd2"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC         NOT =   ZERO )
               MOVE    "4010"          TO  SPA-ERRCD
           END-IF
      *
           .
       4801-PTTEIKIRRK-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    転科歴退避処理
      *****************************************************************
       4801-TENKAREKI-TAIHI-SEC    SECTION.
      *
           INITIALIZE  WRK-TENKA-SRYKA-TBL
      *
      *    入院履歴マスタ検索
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-I01-PTRRKNUM
                                       TO  PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           PERFORM UNTIL ( MCP-RC      NOT =   ZERO )
                   OR    ( WRK-TENKA-SRYKA-CNT
                                          >=  50 )
      *
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
               MOVE    ZERO            TO  FLG-TENKA-SRYKA
               PERFORM VARYING IDX9    FROM    1   BY  1
                       UNTIL ( IDX9    >   50 )
                        OR   ( WRK-TENKA-SRYKA (IDX9) = SPACE )
                        OR   ( FLG-TENKA-SRYKA NOT    = ZERO  )
      *
                   IF    ( WRK-TENKA-SRYKA (IDX9)
                                       =   PTNYUINRRK-NYUINKA )
                       MOVE    1       TO  FLG-TENKA-SRYKA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( FLG-TENKA-SRYKA =   ZERO )
                   COMPUTE WRK-TENKA-SRYKA-CNT
                       =   WRK-TENKA-SRYKA-CNT +   1
                   MOVE    PTNYUINRRK-NYUINKA
                                       TO  WRK-TENKA-SRYKA
                                               (WRK-TENKA-SRYKA-CNT)
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4801-TENKAREKI-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    他院歴通算日数減算更新処理
      *****************************************************************
       4801-TAINTUSANMINUS-UPD-SEC  SECTION.
      *
           MOVE    1           TO      IDXM
           IF    ( SPA-NAI-I01-MAEIDOYMD   NOT =   SPACE )
               MOVE    2       TO      IDXM
           END-IF
      *
           MOVE    SPA-NAI-I01-TRRKNUM (IDXM)
                               TO      WRK-RRKNUM
           MOVE    1           TO      WRK-RRKEDANUM
           PERFORM 4801-PTNYUINRRK-SEL-SEC
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
               MOVE    "4028"  TO      SPA-ERRCD
               GO  TO  4801-TAINTUSANMINUS-UPD-EXT
           END-IF
      *
           IF    ( PTNYUINRRK-JTIKBN   NOT =   "5" )
               MOVE    "4030"  TO  SPA-ERRCD
           END-IF
      *
           IF      PTNYUINRRK-TAISYONISSU  =
                              PTNYUINRRK-TOKU-TAISYONISSU
               COMPUTE PTNYUINRRK-TOKU-TAISYONISSU
                   =   PTNYUINRRK-TOKU-TAISYONISSU  -   1
           END-IF
      *
           COMPUTE PTNYUINRRK-TAISYONISSU
               =   PTNYUINRRK-TAISYONISSU  -   1
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  PTNYUINRRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTNYUINRRK-UPHMS
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4029"  TO  SPA-ERRCD
           END-IF
      *
           .
       4801-TAINTUSANMINUS-UPD-EXT.
           EXIT.
      *****************************************************************
      *    異動前歴更新処理
      *****************************************************************
       4801-IDO-MAE-UPD-SEC  SECTION.
      *
      *    入院履歴マスタ検索
           PERFORM 4802-PTNYUINRRK-SEL-SEC
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
               MOVE    "4013"  TO  SPA-ERRCD
               GO  TO  4801-IDO-MAE-UPD-EXT
           END-IF
      *
           MOVE    ALL "9"     TO  PTNYUINRRK-TENSTUYMD
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  PTNYUINRRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTNYUINRRK-UPHMS
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4014"  TO  SPA-ERRCD
           END-IF
      *
           .
       4801-IDO-MAE-UPD-EXT.
           EXIT.
      *****************************************************************
      *    転出元歴更新処理
      *****************************************************************
       4801-TENSYUTU-MOTO-UPD-SEC  SECTION.
      *
      *    入院履歴マスタ検索
           PERFORM 4801-PTNYUINRRK-SEL-SEC
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
               MOVE    "4015"  TO  SPA-ERRCD
               GO  TO  4801-TENSYUTU-MOTO-UPD-EXT
           END-IF
      *
           MOVE    SPA-NAI-I01-IDOYMD
                               TO  PTNYUINRRK-TENSTUYMD
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  PTNYUINRRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTNYUINRRK-UPHMS
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4016"  TO  SPA-ERRCD
               GO  TO  4801-TENSYUTU-MOTO-UPD-EXT
           END-IF
      *
      *    更新データ用に入院科等を退避
           MOVE    PTNYUINRRK-NYUINKA
                               TO  WRK-NYUINKA-KEY
           MOVE    PTNYUINRRK-BTUNUM
                               TO  WRK-BTUNUM-KEY
           MOVE    PTNYUINRRK-BRMNUM
                               TO  WRK-BRMNUM-KEY
           MOVE    PTNYUINRRK-TOKUTEI-KBN
                               TO  WRK-TOKUTEI-KBN-KEY
           MOVE    PTNYUINRRK-HKNCOMBINUM
                               TO  WRK-HKNCOMBI-KEY
           MOVE    PTNYUINRRK-TOKUTEI-NYUIN
                               TO  WRK-TOKUTEI-NYUIN-KEY
      *
           .
       4801-TENSYUTU-MOTO-UPD-EXT.
           EXIT.
      *****************************************************************
      *    入院歴番号採番処理
      *****************************************************************
       4801-RRKNUM-EDIT-SEC        SECTION.
      *
           EVALUATE    TRUE
      *    入院
           WHEN  ( SPA-GMN-I01-SYORIKBN-CD     =   "01" )
            AND  ( SPA-NAI-I01-MAEIDOYMD       =   SPACE )
      *
               MOVE    ZERO                TO  FLG-PTNYUINRRK
               INITIALIZE  PTNYUINRRK-REC
               MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
               MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC      =   ZERO )
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
                   COMPUTE     WRK-RRKNUM      =   1
                   COMPUTE     WRK-RRKEDANUM   =   1
               ELSE
                   COMPUTE     WRK-RRKNUM      =   PTNYUINRRK-RRKNUM
                                               +   1
                   COMPUTE     WRK-RRKEDANUM   =   1
               END-IF
      *
      *    転科　転棟　転室
           WHEN   ( SPA-GMN-I01-SYORIKBN-CD    =  "08" )
               COMPUTE     WRK-RRKEDANUM   =   WRK-RRKEDANUM   +   1
      *    その他
           WHEN    OTHER
               CONTINUE
           END-EVALUATE
      *
           .
       4801-RRKNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       4801-PTNYUINRRK-SEL-SEC     SECTION.
      *
      *    入院履歴検索
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    WRK-RRKNUM          TO  PTNYUINRRK-RRKNUM
           MOVE    WRK-RRKEDANUM       TO  PTNYUINRRK-RRKEDANUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4801-PTNYUINRRK-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       4802-PTNYUINRRK-SEL-SEC     SECTION.
      *
      *    入院履歴検索
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    WRK-RRKNUM          TO  PTNYUINRRK-RRKNUM
           COMPUTE PTNYUINRRK-RRKEDANUM
               =   WRK-RRKEDANUM       -   1
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4801-PTNYUINRRK-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病室番号編集処理
      *****************************************************************
       4800-BRMNUM-EDIT-SEC        SECTION.
      *
           INITIALIZE                  ORCSBRMNUMAREA
           MOVE    "2"             TO  SBRMNUM-IN-PROCKBN
           MOVE    WRK-BRMNUM      TO  SBRMNUM-IN-BRMNUM
           CALL    "ORCSBRMNUM"    USING   ORCSBRMNUMAREA
           MOVE    SBRMNUM-OT-BRMNUM
                                   TO  WRK-BRMNUM-EDT
      *
           .
       4800-BRMNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院　会計テーブル編集処理
      *****************************************************************
       4801-NYUKAIKEI-EDIT-SEC         SECTION.
      *
      *---------------退院日の同日入院処理ここから----------------------
           MOVE   SPACE                TO  WRK-DOU-TENNYUYMD
      *    同日再入院用に入院日（転入日）を退避
           MOVE   SPA-NAI-I01-NYUINYMD TO  WRK-DOU-NAI-I01-NYUINYMD
           MOVE   PTNYUINRRK-TENNYUYMD TO  WRK-DOU-PTNYUINRRK-TENNYUYMD
      *    入院履歴の退避
           MOVE   PTNYUINRRK-REC       TO  WRKRRK-REC
      *    最終入院履歴が自院歴かを判定する
           MOVE    ZERO                TO  FLG-JIINRRK
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM  910-DBFETCH-SEC
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               IF      PTNYUINRRK-JTIKBN   NOT =   "5"  AND "6"
                   IF  PTNYUINRRK-TAIINYMD  = WRK-DOU-NAI-I01-NYUINYMD                   
                       MOVE   1        TO  FLG-JIINRRK
                   END-IF
               END-IF
           END-IF
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *    入院履歴の復元
           MOVE    WRKRRK-REC          TO  PTNYUINRRK-REC
      *    前歴が自院歴（同日退院分）の場合、入院会計作成日を１日ずらす
           IF     FLG-JIINRRK    =     1
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    1               TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING     STS-AREA-DAY
                                                 LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  SPA-NAI-I01-NYUINYMD
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    WRK-DOU-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    1               TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING     STS-AREA-DAY
                                                 LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  WRK-DOU-TENNYUYMD
           END-IF
      *---------------退院日の同日入院処理ここまで----------------------
      *
           INITIALIZE      ORCSSYOKIKSNAREA
      *
      *    通算日数、算定開始日を求める
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( SPA-ERRCD    NOT =  SPACE )
               GO  TO  4801-NYUKAIKEI-EDIT-EXT
           END-IF
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
      *
      *    同日再入院
           IF     (FLG-JIINRRK           =     1)   AND
                  (PTNYUINRRK-SHOKAIKBN  =    "1")
               ADD   1             TO  SYOKIKSN-IN-TUSANNISSU
           END-IF
      *
      *    介護病棟からの急性増悪による入院時
           IF      PTNYUINRRK-KAIGOKBN    =    "2"
               MOVE    ZERO        TO  SYOKIKSN-IN-TUSANNISSU
               MOVE    ZERO        TO  WRK-TSUSAN
          END-IF
      *
           MOVE    WRK-SANTEISTYMD
                                   TO  SYOKIKSN-IN-SANTEISTYMD
      *
           IF      SPA-GMN-I01-TOKNYUIN-CD    NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
      *        特定一般病棟入院料の場合は期間加算が存在するため、
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        療養病床で有床診療所入院基本料を相互算定する場合も
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        特定入院料の算定要件外に算定にする以下入院料を追加（平成２６年改正で対応）
      *        ・一般病棟特別入院基本料
      *        ・精神病棟１５対１入院基本料
      *        ・精神病棟特別入院基本料
      *        以下入院料を追加（平成２８年改正で対応）
      *        ・療養病棟・一般病棟７対１入院基本料
      *        ・療養病棟・一般病棟１０対１入院基本料
      *        ・療養病棟・一般病棟１３対１入院基本料
      *        ・療養病棟・一般病棟１５対１入院基本料
      *        以下入院料を追加（平成３０年改正で対応）
      *        ・療養病棟・急性期一般入院料２
      *        ・療養病棟・急性期一般入院料３
      *        ・療養病棟・急性期一般入院料４
      *        ・療養病棟・急性期一般入院料５
      *        ・療養病棟・急性期一般入院料６
      *        ・療養病棟・地域一般入院料１
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"   OR
                                                "099999955"   OR
                                                "099999956"   OR
                                                "099999957"   OR
      *                                         特定入院料の算定要件外の算定
                                                "190079010"   OR
                                                "190083810"   OR
                                                "190085010"   OR
      *                                         平成２８年改正対応
                                                "190198110"   OR
                                                "190198210"   OR
                                                "190198310"   OR
                                                "190198410"   OR
      *                                         平成３０年改正対応
                                                "190201910"   OR
                                                "190202010"   OR
                                                "190202110"   OR
                                                "190202210"   OR
                                                "190202310"   OR
                                                "190202410"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4017"      TO  SPA-ERRCD
                       GO  TO  4801-NYUKAIKEI-EDIT-EXT
                   END-IF
               END-IF
           ELSE
               IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM   4801-RJNCD-CHG-SEC
                   PERFORM   4801-YAKINGEN-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4017"      TO  SPA-ERRCD
                       GO  TO  4801-NYUKAIKEI-EDIT-EXT
                   END-IF
               ELSE
                   MOVE    "4018"      TO  SPA-ERRCD
                   GO  TO  4801-NYUKAIKEI-EDIT-EXT
               END-IF
           END-IF
      *
      *    入院会計作成サブの呼出し
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     "1"     TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                            TO   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                            TO   LNK-SCNYUACCT-STSRYYMD
           PERFORM   VARYING   IDX6   FROM   1   BY   1
               UNTIL (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE) OR
                     (IDX6   >     7)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX6)
           END-PERFORM
      *
      *    医療観察入院セット
           INITIALIZE            ORCSIRYOKNS-AREA
           MOVE     PTNYUINRRK-HOSPNUM   TO  LNK-IRYOKNS-HOSPNUM
           MOVE     PTNYUINRRK-TOKUTEI-KBN
                                         TO  LNK-IRYOKNS-KBN
           MOVE     PTNYUINRRK-TOKUTEI-NYUIN
                                         TO  LNK-IRYOKNS-NYUIN
           MOVE     PTNYUINRRK-BTUNUM    TO  LNK-IRYOKNS-BTUNUM
      *     DISPLAY  "==  ORCSIRYOKNS NYUIN-CALL ==="
           MOVE     SYOKIKSN-IN-SANTEISTYMD(1:6)
                                         TO  LNK-IRYOKNS-SANTEIYM
           CALL     "ORCSIRYOKNS"   USING   ORCSIRYOKNS-AREA
                                            SPA-AREA
      D    DISPLAY   "LNK-IRYOKNS-RC = " LNK-IRYOKNS-RC
           IF       LNK-IRYOKNS-RC     =    ZERO
               MOVE   LNK-IRYOKNS-SRYCD(1)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   LNK-IRYOKNS-SRYCD(2)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(2)
               MOVE   LNK-IRYOKNS-SRYCD(3)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(3)
               MOVE   LNK-IRYOKNS-SRYCD(4)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(4)
               MOVE   LNK-IRYOKNS-SRYCD(5)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(5)
               MOVE   SPA-NAI-I01-NYUINYMD
                                  TO  LNK-SCNYUACCT-STSRYYMD
               MOVE    "99999999"
                                  TO  LNK-SCNYUACCT-EDSRYYMD
           END-IF
      *
      *    通算算定済日数
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    特定入院で入院料が変更になるコードの対応
      *
      *    ===  TEST NYUIN-TOUROKU ===
           INITIALIZE                       ORCSTOKNYUIN2-AREA
           MOVE   PTNYUINRRK-HOSPNUM    TO  LNK-TOKNYUIN2-HOSPNUM
           MOVE   PTNYUINRRK-PTID       TO  LNK-TOKNYUIN2-PTID
           MOVE   "1"                   TO  LNK-TOKNYUIN2-IDOKBN
           MOVE   SPA-NAI-I01-NYUINYMD  TO  LNK-TOKNYUIN2-NYUINYMD
           MOVE   SPA-NAI-I01-NYUINYMD  TO  LNK-TOKNYUIN2-SANTEIYMD
           MOVE   SYOKIKSN-IN-KHNSRYCD  TO  LNK-TOKNYUIN2-SET-SRYCD
           MOVE   STUSAN-NISSU08        TO  LNK-TOKNYUIN2-NISSU08
           MOVE   PTNYUINRRK-SHONUM     TO  LNK-TOKNYUIN2-SHONUM
           MOVE   PTNYUINRRK-TOKUTEI-NYUIN
                             TO  LNK-TOKNYUIN2-TOKUTEI-NYUIN
           CALL  "ORCSTOKNYUIN2"     USING  ORCSTOKNYUIN2-AREA
                                            SPA-AREA
      *     DISPLAY  "LNK-TOKNYUIN2-RC = " LNK-TOKNYUIN2-RC
           IF     LNK-TOKNYUIN2-RC      =   ZERO
              MOVE   SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                       LNK-SCNYUACCT-NYUKHN-SRYCD(1)
              PERFORM   VARYING   IDX    FROM    1   BY  1
                UNTIL   IDX       >    5
                  IF   LNK-TOKNYUIN2-SRYCD(IDX)  NOT =   SPACE
                    MOVE  LNK-TOKNYUIN2-SRYCD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                    MOVE  LNK-TOKNYUIN2-STYMD(IDX)
                                TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX)
                    MOVE  LNK-TOKNYUIN2-EDYMD(IDX)
                                TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX)
                  END-IF
              END-PERFORM
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                        TO  IPN-NYUKAIKEI-SAKUSEI-FLG
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                        TO  IPN-SANTEISTYMD
           ELSE
              IF   PTNYUINRRK-TOKUTEI-NYUIN  NOT =   SPACE
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                        TO  IPN-NYUKAIKEI-SAKUSEI-FLG
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                        TO  IPN-SANTEISTYMD
              END-IF
           END-IF
      *
      *
      *-------特定入院対応ここまで-----------------------------------
      *
      *    入院料加算診療コード取り込み
           PERFORM    4801-NYUKSN-GET-SEC
      *
      *    特定入院料区分のセット
           IF      SPA-GMN-I01-TOKNYUIN-CD    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        特定入院基本料、療養病棟入院基本料、療養病床入院基本料等は
      *        特定入院料扱いから外す
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
      *            特定入院基本料（一般病棟入院基本料）はマスタが
      *            平成２６年９月３０日までの為、参照時の日付を変更
                   IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                       MOVE    "20140930"   TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   END-IF
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
               IF    LNK-SCNYUACCT-KAGEN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *    外泊
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9
                                TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    食事区分
           MOVE   "099999913"   TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           EVALUATE   PTNYUINRRK-SKJKBN
               WHEN   "0"
                   MOVE   0     TO  LNK-SCNYUACCT-SHOKUJIKBN
               WHEN   "1"
                   MOVE   1     TO  LNK-SCNYUACCT-SHOKUJIKBN
               WHEN   "2"
                   MOVE   2     TO  LNK-SCNYUACCT-SHOKUJIKBN
               WHEN   "3"
                   MOVE   3     TO  LNK-SCNYUACCT-SHOKUJIKBN
               WHEN   "C"
                   MOVE  "1"    TO  LNK-SCNYUACCT-SHOKUJI-SETKBN
                   MOVE  PTNYUINRRK-SKJKBN-ASA
                                TO  LNK-SCNYUACCT-SHOKUJIKBN-ASA
                   MOVE  PTNYUINRRK-SKJKBN-HIRU
                                TO  LNK-SCNYUACCT-SHOKUJIKBN-HIRU
                   MOVE  PTNYUINRRK-SKJKBN-YU
                                TO  LNK-SCNYUACCT-SHOKUJIKBN-YORU
           END-EVALUATE
      *    食堂加算
           PERFORM     4801-BTUSEL-SEC
           IF   FLG-SYSKANRI           =    ZERO
               IF   SYS-5001-BTU-SYOKUDOKBN       =    "2"
                    MOVE     "197000570"    TO
                                LNK-SCNYUACCT-SHOKUDO-SRYCD
                    MOVE    1   TO   LNK-SCNYUACCT-SHOKUDO-KBN
                    IF   PTNYUINRRK-SKJKBN   =   "0"
                         MOVE   ZERO     TO  LNK-SCNYUACCT-SHOKUDO-KBN
                    END-IF
                    IF  (PTNYUINRRK-SKJKBN      =   "C")   AND
                        (PTNYUINRRK-SKJKBN-ASA  =   "0")   AND
                        (PTNYUINRRK-SKJKBN-HIRU =   "0")   AND
                        (PTNYUINRRK-SKJKBN-YU   =   "0")
                         MOVE   ZERO     TO  LNK-SCNYUACCT-SHOKUDO-KBN
                    END-IF
               END-IF
           END-IF
      *    保険組合せ
           MOVE   "099999914"   TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE   PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           IF     PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
      *    入院カレンダー未設定の判定
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え
           IF    (LNK-SCNYUACCT-STSRYYMD   >=   "20120401")  AND
                 (LNK-SCNYUACCT-STSRYYMD   <=   "20140331")
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え（平成２６年改定）
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20140401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169610"
                             MOVE "190170810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169710"
                             MOVE "190170910" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169810"
                             MOVE "190171010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169910"
                             MOVE "190171110" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170010"
                             MOVE "190171210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170110"
                             MOVE "190171310" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170210"
                             MOVE "190171410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170310"
                             MOVE "190171510" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170410"
                             MOVE "190171610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    特定入院基本料のコード細分化置き換え
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161010" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161210" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
      *
      *            特定機能病院は病棟設定された入院コードから判定
                   IF   SYS-5001-BTU-KHNSRYCD =  "190118610" OR
                                                 "190705610" OR
                                                 "190085910"
                     MOVE "190160810" TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-IF
               END-IF
      *        特別入院基本料
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799510"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161110" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161310" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE   PTNYUINRRK-BTUNUM   TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
               DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                         LNK-SCNYUACCT-RC
           END-IF
      *
      *-------入院時食事時間区分対応ここから-----------------------------------
      *     DISPLAY "PTNYUINRRK-DOUJITSUKBN = " PTNYUINRRK-DOUJITSUKBN
      *     DISPLAY "PTNYUINRRK-TIMEKBN-IN = " PTNYUINRRK-TIMEKBN-IN
      *     DISPLAY "PTNYUINRRK-NYUINYMD = " PTNYUINRRK-NYUINYMD
      *     DISPLAY "PTNYUINRRK-TENNYUYMD = " PTNYUINRRK-TENNYUYMD
      *    同日再入院でない、又は他院歴からの同日再入院の場合は、
      *    入院時食事時間区分に従い入院会計データを更新する。
           IF   PTNYUINRRK-NYUINYMD      >=   "20060401"
             IF   PTNYUINRRK-SKJKBN       =    "1" OR "2" OR "3" OR "C"
               IF   PTNYUINRRK-DOUJITSUKBN     =   SPACE  OR  "2"
      *            通常の入院又は他院歴からの同日入院
      *
      *            食事なし
                   IF   PTNYUINRRK-TIMEKBN-IN    =    "0"
      *                朝食をクリア
                       MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                       MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                       MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                                   TO  NACCT-SRYYM
                       MOVE    "key30"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-SELECT-SEC
                       MOVE    "key30"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-READ-SEC
                       IF      FLG-NYUINACCT        =   ZERO
                           MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                           MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                              TO  IDX-SHOKUJIX
                           MOVE     ZERO   TO  NACCT-DAY(IDX-SHOKUJI9)
      *                    剤回数再セット
                           MOVE  ZERO              TO  NACCT-ZAIKAISU
                           PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                    UNTIL     IDX-SKJ   >   31
                                IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                                   ADD   1        TO  NACCT-ZAIKAISU
                                END-IF
                           END-PERFORM   
                           MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                           MOVE     "tbl_nyuinacct" TO MCP-TABLE
                           MOVE     "key"          TO  MCP-PATHNAME
                           PERFORM  910-DBUPDATE-SEC
      *                    入院会計マスタクローズ
                           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                           MOVE    "key30"     TO  MCP-PATHNAME
                           PERFORM  910-DBCLOSECURSOR-SEC
                       END-IF
      *                昼食をクリア
                       MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                       MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                       MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                                   TO  NACCT-SRYYM
                       MOVE    "key31"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-SELECT-SEC
                       MOVE    "key31"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-READ-SEC
                       IF      FLG-NYUINACCT        =   ZERO
                           MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                           MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                              TO  IDX-SHOKUJIX
                           MOVE     ZERO   TO  NACCT-DAY(IDX-SHOKUJI9)
      *                    剤回数再セット
                           MOVE  ZERO              TO  NACCT-ZAIKAISU
                           PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                    UNTIL     IDX-SKJ   >   31
                                IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                                   ADD   1        TO  NACCT-ZAIKAISU
                                END-IF
                           END-PERFORM   
                           MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                           MOVE     "tbl_nyuinacct" TO MCP-TABLE
                           MOVE     "key"          TO  MCP-PATHNAME
                           PERFORM  910-DBUPDATE-SEC
      *                    入院会計マスタクローズ
                           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                           MOVE    "key31"     TO  MCP-PATHNAME
                           PERFORM  910-DBCLOSECURSOR-SEC
                       END-IF
      *                夕食をクリア
                       MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                       MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                       MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                                   TO  NACCT-SRYYM
                       MOVE    "key32"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-SELECT-SEC
                       MOVE    "key32"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-READ-SEC
                       IF      FLG-NYUINACCT        =   ZERO
                           MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                           MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                              TO  IDX-SHOKUJIX
                           MOVE     ZERO   TO  NACCT-DAY(IDX-SHOKUJI9)
      *                    剤回数再セット
                           MOVE  ZERO              TO  NACCT-ZAIKAISU
                           PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                    UNTIL     IDX-SKJ   >   31
                                IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                                   ADD   1        TO  NACCT-ZAIKAISU
                                END-IF
                           END-PERFORM   
                           MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                           MOVE     "tbl_nyuinacct" TO MCP-TABLE
                           MOVE     "key"          TO  MCP-PATHNAME
                           PERFORM  910-DBUPDATE-SEC
      *                    入院会計マスタクローズ
                           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                           MOVE    "key32"     TO  MCP-PATHNAME
                           PERFORM  910-DBCLOSECURSOR-SEC
                       END-IF
                   END-IF
      *
      *            昼食から
                   IF   PTNYUINRRK-TIMEKBN-IN    =    "2"
      *                朝食をクリア
                       MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                       MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                       MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                                   TO  NACCT-SRYYM
                       MOVE    "key30"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-SELECT-SEC
                       MOVE    "key30"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-READ-SEC
                       IF      FLG-NYUINACCT        =   ZERO
                           MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                           MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                              TO  IDX-SHOKUJIX
                           MOVE     ZERO   TO  NACCT-DAY(IDX-SHOKUJI9)
      *                    剤回数再セット
                           MOVE  ZERO              TO  NACCT-ZAIKAISU
                           PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                    UNTIL     IDX-SKJ   >   31
                                IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                                   ADD   1        TO  NACCT-ZAIKAISU
                                END-IF
                           END-PERFORM   
                           MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                           MOVE     "tbl_nyuinacct" TO MCP-TABLE
                           MOVE     "key"          TO  MCP-PATHNAME
                           PERFORM  910-DBUPDATE-SEC
      *                    入院会計マスタクローズ
                           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                           MOVE    "key30"     TO  MCP-PATHNAME
                           PERFORM  910-DBCLOSECURSOR-SEC
                       END-IF
                   END-IF
      *
      *            夕食から
                   IF   PTNYUINRRK-TIMEKBN-IN    =    "3"
      *                朝食をクリア
                       MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                       MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                       MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                                   TO  NACCT-SRYYM
                       MOVE    "key30"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-SELECT-SEC
                       MOVE    "key30"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-READ-SEC
                       IF      FLG-NYUINACCT        =   ZERO
                           MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                           MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                              TO  IDX-SHOKUJIX
                           MOVE     ZERO   TO  NACCT-DAY(IDX-SHOKUJI9)
      *                    剤回数再セット
                           MOVE  ZERO              TO  NACCT-ZAIKAISU
                           PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                    UNTIL     IDX-SKJ   >   31
                                IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                                   ADD   1        TO  NACCT-ZAIKAISU
                                END-IF
                           END-PERFORM   
                           MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                           MOVE     "tbl_nyuinacct" TO MCP-TABLE
                           MOVE     "key"          TO  MCP-PATHNAME
                           PERFORM  910-DBUPDATE-SEC
      *                    入院会計マスタクローズ
                           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                           MOVE    "key30"     TO  MCP-PATHNAME
                           PERFORM  910-DBCLOSECURSOR-SEC
                       END-IF
      *                昼食をクリア
                       MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                       MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                       MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                                   TO  NACCT-SRYYM
                       MOVE    "key31"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-SELECT-SEC
                       MOVE    "key31"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-READ-SEC
                       IF      FLG-NYUINACCT        =   ZERO
                           MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                           MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                              TO  IDX-SHOKUJIX
                           MOVE     ZERO   TO  NACCT-DAY(IDX-SHOKUJI9)
      *                    剤回数再セット
                           MOVE  ZERO              TO  NACCT-ZAIKAISU
                           PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                    UNTIL     IDX-SKJ   >   31
                                IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                                   ADD   1        TO  NACCT-ZAIKAISU
                                END-IF
                           END-PERFORM   
                           MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                           MOVE     "tbl_nyuinacct" TO MCP-TABLE
                           MOVE     "key"          TO  MCP-PATHNAME
                           PERFORM  910-DBUPDATE-SEC
      *                    入院会計マスタクローズ
                           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                           MOVE    "key31"     TO  MCP-PATHNAME
                           PERFORM  910-DBCLOSECURSOR-SEC
                       END-IF
                   END-IF
               ELSE
      *            自院退院分からの同日入退院（歴作成分は除く）
      *
      *            朝食から
                   IF   PTNYUINRRK-TIMEKBN-IN    =    "1"
      *                朝食をセット
                       MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                       MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                       MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                                   TO  NACCT-SRYYM
                       MOVE    "key30"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-SELECT-SEC
                       MOVE    "key30"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-READ-SEC
                       IF      FLG-NYUINACCT        =   ZERO
                           MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                           MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                              TO  IDX-SHOKUJIX
                           IF   PTNYUINRRK-SKJKBN        =    "1"
                               MOVE  1   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "2"
                               MOVE  2   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "3"
                               MOVE  3   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "C"
                              IF   PTNYUINRRK-SKJKBN-ASA    =    "1"
                                  MOVE  1   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                              IF   PTNYUINRRK-SKJKBN-ASA    =    "2"
                                  MOVE  2   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                              IF   PTNYUINRRK-SKJKBN-ASA    =    "3"
                                  MOVE  3   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                           END-IF
      *                    剤回数再セット
                           MOVE  ZERO              TO  NACCT-ZAIKAISU
                           PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                    UNTIL     IDX-SKJ   >   31
                                IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                                   ADD   1        TO  NACCT-ZAIKAISU
                                END-IF
                           END-PERFORM   
                           MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                           MOVE     "tbl_nyuinacct" TO MCP-TABLE
                           MOVE     "key"          TO  MCP-PATHNAME
                           PERFORM  910-DBUPDATE-SEC
      *                    入院会計マスタクローズ
                           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                           MOVE    "key30"     TO  MCP-PATHNAME
                           PERFORM  910-DBCLOSECURSOR-SEC
                       END-IF
      *                昼食をセット
                       MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                       MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                       MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                                   TO  NACCT-SRYYM
                       MOVE    "key31"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-SELECT-SEC
                       MOVE    "key31"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-READ-SEC
                       IF      FLG-NYUINACCT        =   ZERO
                           MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                           MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                              TO  IDX-SHOKUJIX
                           IF   PTNYUINRRK-SKJKBN        =    "1"
                               MOVE  1   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "2"
                               MOVE  2   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "3"
                               MOVE  3   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "C"
                              IF   PTNYUINRRK-SKJKBN-HIRU   =    "1"
                                  MOVE  1   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                              IF   PTNYUINRRK-SKJKBN-HIRU   =    "2"
                                  MOVE  2   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                              IF   PTNYUINRRK-SKJKBN-HIRU   =    "3"
                                  MOVE  3   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                           END-IF
      *                    剤回数再セット
                           MOVE  ZERO              TO  NACCT-ZAIKAISU
                           PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                    UNTIL     IDX-SKJ   >   31
                                IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                                   ADD   1        TO  NACCT-ZAIKAISU
                                END-IF
                           END-PERFORM   
                           MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                           MOVE     "tbl_nyuinacct" TO MCP-TABLE
                           MOVE     "key"          TO  MCP-PATHNAME
                           PERFORM  910-DBUPDATE-SEC
      *                    入院会計マスタクローズ
                           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                           MOVE    "key31"     TO  MCP-PATHNAME
                           PERFORM  910-DBCLOSECURSOR-SEC
                       END-IF
      *                夕食をセット
                       MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                       MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                       MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                                   TO  NACCT-SRYYM
                       MOVE    "key32"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-SELECT-SEC
                       MOVE    "key32"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-READ-SEC
                       IF      FLG-NYUINACCT        =   ZERO
                           MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                           MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                              TO  IDX-SHOKUJIX
                           IF   PTNYUINRRK-SKJKBN        =    "1"
                               MOVE  1   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "2"
                               MOVE  2   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "3"
                               MOVE  3   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "C"
                              IF   PTNYUINRRK-SKJKBN-YU     =    "1"
                                  MOVE  1   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                              IF   PTNYUINRRK-SKJKBN-YU     =    "2"
                                  MOVE  2   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                              IF   PTNYUINRRK-SKJKBN-YU     =    "3"
                                  MOVE  3   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                           END-IF
      *                    剤回数再セット
                           MOVE  ZERO              TO  NACCT-ZAIKAISU
                           PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                    UNTIL     IDX-SKJ   >   31
                                IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                                   ADD   1        TO  NACCT-ZAIKAISU
                                END-IF
                           END-PERFORM   
                           MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                           MOVE     "tbl_nyuinacct" TO MCP-TABLE
                           MOVE     "key"          TO  MCP-PATHNAME
                           PERFORM  910-DBUPDATE-SEC
      *                    入院会計マスタクローズ
                           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                           MOVE    "key32"     TO  MCP-PATHNAME
                           PERFORM  910-DBCLOSECURSOR-SEC
                       END-IF
                   END-IF
      *
      *            昼食から
                   IF   PTNYUINRRK-TIMEKBN-IN    =    "2"
      *                昼食をセット
                       MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                       MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                       MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                                   TO  NACCT-SRYYM
                       MOVE    "key31"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-SELECT-SEC
                       MOVE    "key31"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-READ-SEC
                       IF      FLG-NYUINACCT        =   ZERO
                           MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                           MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                              TO  IDX-SHOKUJIX
                           IF   PTNYUINRRK-SKJKBN        =    "1"
                               MOVE  1   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "2"
                               MOVE  2   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "3"
                               MOVE  3   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "C"
                              IF   PTNYUINRRK-SKJKBN-HIRU   =    "1"
                                  MOVE  1   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                              IF   PTNYUINRRK-SKJKBN-HIRU   =    "2"
                                  MOVE  2   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                              IF   PTNYUINRRK-SKJKBN-HIRU   =    "3"
                                  MOVE  3   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                           END-IF
      *                    剤回数再セット
                           MOVE  ZERO              TO  NACCT-ZAIKAISU
                           PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                    UNTIL     IDX-SKJ   >   31
                                IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                                   ADD   1        TO  NACCT-ZAIKAISU
                                END-IF
                           END-PERFORM   
                           MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                           MOVE     "tbl_nyuinacct" TO MCP-TABLE
                           MOVE     "key"          TO  MCP-PATHNAME
                           PERFORM  910-DBUPDATE-SEC
      *                    入院会計マスタクローズ
                           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                           MOVE    "key31"     TO  MCP-PATHNAME
                           PERFORM  910-DBCLOSECURSOR-SEC
                       END-IF
      *                夕食をセット
                       MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                       MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                       MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                                   TO  NACCT-SRYYM
                       MOVE    "key32"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-SELECT-SEC
                       MOVE    "key32"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-READ-SEC
                       IF      FLG-NYUINACCT        =   ZERO
                           MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                           MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                              TO  IDX-SHOKUJIX
                           IF   PTNYUINRRK-SKJKBN        =    "1"
                               MOVE  1   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "2"
                               MOVE  2   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "3"
                               MOVE  3   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "C"
                              IF   PTNYUINRRK-SKJKBN-YU     =    "1"
                                  MOVE  1   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                              IF   PTNYUINRRK-SKJKBN-YU     =    "2"
                                  MOVE  2   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                              IF   PTNYUINRRK-SKJKBN-YU     =    "3"
                                  MOVE  3   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                           END-IF
      *                    剤回数再セット
                           MOVE  ZERO              TO  NACCT-ZAIKAISU
                           PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                    UNTIL     IDX-SKJ   >   31
                                IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                                   ADD   1        TO  NACCT-ZAIKAISU
                                END-IF
                           END-PERFORM   
                           MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                           MOVE     "tbl_nyuinacct" TO MCP-TABLE
                           MOVE     "key"          TO  MCP-PATHNAME
                           PERFORM  910-DBUPDATE-SEC
      *                    入院会計マスタクローズ
                           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                           MOVE    "key32"     TO  MCP-PATHNAME
                           PERFORM  910-DBCLOSECURSOR-SEC
                       END-IF
                   END-IF
      *
      *            夕食から
                   IF   PTNYUINRRK-TIMEKBN-IN    =    "3"
      *                夕食をセット
                       MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                       MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                       MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                                   TO  NACCT-SRYYM
                       MOVE    "key32"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-SELECT-SEC
                       MOVE    "key32"             TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-READ-SEC
                       IF      FLG-NYUINACCT        =   ZERO
                           MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                           MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                              TO  IDX-SHOKUJIX
                           IF   PTNYUINRRK-SKJKBN        =    "1"
                               MOVE  1   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "2"
                               MOVE  2   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "3"
                               MOVE  3   TO  NACCT-DAY(IDX-SHOKUJI9)
                           END-IF
                           IF   PTNYUINRRK-SKJKBN        =    "C"
                              IF   PTNYUINRRK-SKJKBN-YU     =    "1"
                                  MOVE  1   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                              IF   PTNYUINRRK-SKJKBN-YU     =    "2"
                                  MOVE  2   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                              IF   PTNYUINRRK-SKJKBN-YU     =    "3"
                                  MOVE  3   TO  NACCT-DAY(IDX-SHOKUJI9)
                              END-IF
                           END-IF
      *                    剤回数再セット
                           MOVE  ZERO              TO  NACCT-ZAIKAISU
                           PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                    UNTIL     IDX-SKJ   >   31
                                IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                                   ADD   1        TO  NACCT-ZAIKAISU
                                END-IF
                           END-PERFORM   
                           MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                           MOVE     "tbl_nyuinacct" TO MCP-TABLE
                           MOVE     "key"          TO  MCP-PATHNAME
                           PERFORM  910-DBUPDATE-SEC
      *                    入院会計マスタクローズ
                           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                           MOVE    "key32"     TO  MCP-PATHNAME
                           PERFORM  910-DBCLOSECURSOR-SEC
                       END-IF
                   END-IF
               END-IF
             END-IF
           END-IF
      *
      *-------入院時食事時間区分対応ここまで-----------------------------------
      *
      *-------入院当日の食堂加算対応ここから-----------------------------------
           MOVE    ZERO                TO  WRK-SHOKUJI-ASA
           MOVE    ZERO                TO  WRK-SHOKUJI-HIRU
           MOVE    ZERO                TO  WRK-SHOKUJI-YORU
      *    朝食
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                       TO  NACCT-SRYYM
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-SELECT-SEC
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-READ-SEC
           IF      FLG-NYUINACCT        =   ZERO
               MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                       TO  IDX-SHOKUJIX
               IF   NACCT-DAY(IDX-SHOKUJI9)  =  1  OR  2  OR  3
                   MOVE    1           TO  WRK-SHOKUJI-ASA
               END-IF
           END-IF
      *   入院会計マスタクローズ
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key30"     TO  MCP-PATHNAME
           PERFORM  910-DBCLOSECURSOR-SEC
      *
      *    昼食
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                       TO  NACCT-SRYYM
           MOVE    "key31"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-SELECT-SEC
           MOVE    "key31"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-READ-SEC
           IF      FLG-NYUINACCT        =   ZERO
               MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                       TO  IDX-SHOKUJIX
               IF   NACCT-DAY(IDX-SHOKUJI9)  =  1  OR  2  OR  3
                   MOVE    1           TO  WRK-SHOKUJI-HIRU
               END-IF
           END-IF
      *   入院会計マスタクローズ
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key31"     TO  MCP-PATHNAME
           PERFORM  910-DBCLOSECURSOR-SEC
      *
      *    夕食
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                       TO  NACCT-SRYYM
           MOVE    "key32"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-SELECT-SEC
           MOVE    "key32"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-READ-SEC
           IF      FLG-NYUINACCT        =   ZERO
               MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                       TO  IDX-SHOKUJIX
               IF   NACCT-DAY(IDX-SHOKUJI9)  =  1  OR  2  OR  3
                   MOVE    1           TO  WRK-SHOKUJI-YORU
               END-IF
           END-IF
      *   入院会計マスタクローズ
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key32"     TO  MCP-PATHNAME
           PERFORM  910-DBCLOSECURSOR-SEC
      *
      *    食堂加算の再セット
           IF    (WRK-SHOKUJI-ASA      =   ZERO)   AND
                 (WRK-SHOKUJI-HIRU     =   ZERO)   AND
                 (WRK-SHOKUJI-YORU     =   ZERO)
               MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                           TO  NACCT-SRYYM
               MOVE    "key43"             TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-SELECT-SEC
               MOVE    "key43"             TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT        =   ZERO
                   MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                           TO  IDX-SHOKUJIX
                   MOVE    ZERO            TO  NACCT-DAY(IDX-SHOKUJI9)
      *            剤回数再セット
                   MOVE  ZERO              TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                     UNTIL     IDX-SKJ   >   31
                        IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                             ADD   1       TO  NACCT-ZAIKAISU
                        END-IF
                   END-PERFORM
                   MOVE     NYUINACCT-REC   TO  MCPDATA-REC
                   MOVE     "tbl_nyuinacct" TO MCP-TABLE
                   MOVE     "key"           TO  MCP-PATHNAME
                   PERFORM  910-DBUPDATE-SEC
               END-IF
      *        入院会計マスタクローズ
               MOVE    "tbl_nyuinacct" TO  MCP-TABLE
               MOVE    "key43"     TO  MCP-PATHNAME
               PERFORM  910-DBCLOSECURSOR-SEC
           ELSE
             IF    LNK-SCNYUACCT-SHOKUDO-KBN    =    1
               MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    PTNYUINRRK-NYUINYMD(1:6)
                                           TO  NACCT-SRYYM
               MOVE    "key43"             TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-SELECT-SEC
               MOVE    "key43"             TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT        =   ZERO
                   MOVE     PTNYUINRRK-NYUINYMD(7:2)
                                           TO  IDX-SHOKUJIX
                   MOVE    1               TO  NACCT-DAY(IDX-SHOKUJI9)
      *            剤回数再セット
                   MOVE  ZERO              TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                     UNTIL     IDX-SKJ   >   31
                        IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                             ADD   1       TO  NACCT-ZAIKAISU
                        END-IF
                   END-PERFORM
                   MOVE     NYUINACCT-REC   TO  MCPDATA-REC
                   MOVE     "tbl_nyuinacct" TO MCP-TABLE
                   MOVE     "key"           TO  MCP-PATHNAME
                   PERFORM  910-DBUPDATE-SEC
               END-IF
      *        入院会計マスタクローズ
               MOVE    "tbl_nyuinacct" TO  MCP-TABLE
               MOVE    "key43"     TO  MCP-PATHNAME
               PERFORM  910-DBCLOSECURSOR-SEC
             END-IF
           END-IF
      *-------入院当日の食堂加算対応ここまで-----------------------------------
      *
           MOVE   ZERO                   TO   FLG-SAI-SAKUSEI
           MOVE   ZERO                   TO   FLG-SAI-SAKUSEI2
           MOVE   SPACE                  TO   WRK-SAI-I01-TOKNYUIN-CD
      *
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
               MOVE    SPA-GMN-I01-TOKNYUIN-CD
                                         TO   WRK-SAI-I01-TOKNYUIN-CD
               MOVE    SPACE             TO   SPA-GMN-I01-TOKNYUIN-CD
               MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
               MOVE    IPN-SANTEISTYMD   TO   SPA-NAI-I01-IDOYMD
               MOVE    IPN-SANTEISTYMD   TO   PTNYUINRRK-TENNYUYMD
               MOVE    WRK-SANTEISTYMD   TO   SPA-NAI-I01-NYUINYMD
               MOVE    1                 TO   FLG-SAI-SAKUSEI
               PERFORM   4801-TENROOM-EDIT-SEC
           END-IF
      *----入院年月日翌月分の入院会計再作成処理--------------------
      *      DISPLAY  "####  NYUIN-KAIKEI  SAI-SAKUSEI-SHORI ####"
      *      DISPLAY  "LNK-SCNYUACCT-STSRYYMD = "
      *                LNK-SCNYUACCT-STSRYYMD
      *      DISPLAY  "PTNYUINRRK-NYUINYMD = "
      *                PTNYUINRRK-NYUINYMD
      *     会計作成用の年月算出（入院年月＋１月）
            IF      FLG-SAI-SAKUSEI     =   1
                MOVE    PTNYUINRRK-NYUINYMD(1:6)  TO  WRK-SAI-SRYYM
                MOVE    WRK-SAI-I01-TOKNYUIN-CD   TO
                                         SPA-GMN-I01-TOKNYUIN-CD
            ELSE
                MOVE    LNK-SCNYUACCT-STSRYYMD(1:6)  TO  WRK-SAI-SRYYM
            END-IF
            IF      WRK-SAI-SRYMR        =  12
                COMPUTE   WRK-SAI-SRYYR =   WRK-SAI-SRYYR + 1
                MOVE      1             TO  WRK-SAI-SRYMR
            ELSE
                COMPUTE   WRK-SAI-SRYMR =   WRK-SAI-SRYMR + 1
            END-IF
      *      DISPLAY "WRK-SAI-SRYYM = " WRK-SAI-SRYYM
            MOVE    WRK-SAI-SRYYM    TO  WRK-SANTEISTYMD(1:6)
            MOVE    "01"             TO  WRK-SANTEISTYMD(7:2)
            MOVE    WRK-SANTEISTYMD  TO  SPA-NAI-I01-IDOYMD
            MOVE    WRK-SANTEISTYMD  TO  PTNYUINRRK-TENNYUYMD
            MOVE    WRK-SANTEISTYMD  TO  SPA-NAI-I01-NYUINYMD
            MOVE    1                TO  FLG-SAI-SAKUSEI2
            PERFORM 4801-TENROOM-EDIT-SEC
            IF    (IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1")  AND
                  (WRK-SAI-SRYYM(1:6)   =    IPN-SANTEISTYMD(1:6))
                MOVE    SPA-GMN-I01-TOKNYUIN-CD
                                          TO   WRK-SAI-I01-TOKNYUIN-CD
                MOVE    SPACE             TO   SPA-GMN-I01-TOKNYUIN-CD
                MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
                MOVE    IPN-SANTEISTYMD   TO   SPA-NAI-I01-IDOYMD
                MOVE    IPN-SANTEISTYMD   TO   PTNYUINRRK-TENNYUYMD
                MOVE    WRK-SANTEISTYMD   TO   SPA-NAI-I01-NYUINYMD
                MOVE    1                 TO   FLG-SAI-SAKUSEI2
                PERFORM   4801-TENROOM-EDIT-SEC
            END-IF
      *
           .
       4801-NYUKAIKEI-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料加算の取り込み処理
      *****************************************************************
       4801-NYUKSN-GET-SEC         SECTION.
      *
      *    入院料加算取得サブ
           INITIALIZE                       ORCSNYUKSNGET-AREA
           MOVE   SPA-HOSPNUM           TO  LNK-NYUKSNGET-HOSPNUM
           MOVE   PTNYUINRRK-PTID       TO  LNK-NYUKSNGET-PTID
           EVALUATE   SPA-GMN-I01-SYORIKBN-CD
               WHEN   "01"
                   MOVE   SPA-NAI-I01-NYUINYMD(1:6)
                                        TO  LNK-NYUKSNGET-SANTEIYM
               WHEN   "07"
                   MOVE   SPA-NAI-I01-TAIINYMD(1:6)
                                        TO  LNK-NYUKSNGET-SANTEIYM
               WHEN   "08"
                   MOVE   SPA-NAI-I01-IDOYMD(1:6)
                                        TO  LNK-NYUKSNGET-SANTEIYM
               WHEN   "09"
                   MOVE   SPA-NAI-I01-MAEIDOYMD(1:6)
                                        TO  LNK-NYUKSNGET-SANTEIYM
               WHEN   OTHER
                   MOVE    SPA-SYSYMD(1:6)
                                        TO  LNK-NYUKSNGET-SANTEIYM
           END-EVALUATE
           MOVE   PTNYUINRRK-BTUNUM     TO  LNK-NYUKSNGET-BTUNUM
           MOVE   PTNYUINRRK-BRMNUM     TO  LNK-NYUKSNGET-BRMNUM
           IF      LNK-SCNYUACCT-NYUKHN-SRYCD(1)   NOT = SPACE
               MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                        TO  LNK-NYUKSNGET-NYUIN-SRYCD
           ELSE
               MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                        TO  LNK-NYUKSNGET-NYUIN-SRYCD
           END-IF
      *
      *    特定入院料算定要件外で療養病棟入院基本料Ｉを算定時に、入院料
      *    加算を特別判定する場合の判定区分セット
           IF      LNK-TOKNYUIN-GAI-NYUKSNKBN   =    "1"
               MOVE   "1"               TO  LNK-NYUKSNGET-GAI-NYUKSNKBN
           END-IF
      *
           MOVE    ZERO                 TO  IDX
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN      >   CONST-GMN-KSN-MAX )
               IF    ( SPA-GMN-I01-KSN-SRYCD (IDX-KSN)
                                        NOT =   SPACE )
                AND  ( SPA-GMN-I01-KSN-CD (IDX-KSN)     =   "2" )
                    ADD     1           TO     IDX
                    MOVE    SPA-GMN-I01-KSN-SRYCD (IDX-KSN)
                                TO     LNK-NYUKSNGET-I01-KSN-SRYCD(IDX)
               END-IF
           END-PERFORM
           MOVE    FLG-IPPAN            TO  LNK-NYUKSNGET-FLG-IPPAN
           MOVE    FLG-IPPAN2           TO  LNK-NYUKSNGET-FLG-IPPAN2
           CALL    "ORCSNYUKSNGET"      USING  ORCSNYUKSNGET-AREA
                                               SPA-AREA
      *    入院料加算取得サブからの返却値セット
           PERFORM    VARYING    IDX8   FROM   1   BY   1
             UNTIL    (LNK-NYUKSNGET-KSNSRYCD(IDX8)  =  SPACE)  OR
                      (IDX8   >     20)
                 MOVE   LNK-NYUKSNGET-NYUKSNKBN(IDX8)
                             TO  LNK-SCNYUACCT-NYUKHNKSN-KBN(IDX8)
                 MOVE   LNK-NYUKSNGET-KSNSRYCD(IDX8)
                             TO  LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
           END-PERFORM
           MOVE    LNK-NYUKSNGET-TOKUBETSU-NYUIN
                                        TO  WRK-TOKUBETSU-NYUIN
      *
           .
      *
       4801-NYUKSN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院取消　会計テーブル編集処理
      *****************************************************************
       4801-NYUKAIKEI-ERASE-SEC         SECTION.
      *
           PERFORM     VARYING   IDX6   FROM   1   BY   1
                       UNTIL    (WRK-TENKA-SRYKA(IDX6)  =  SPACE)   OR
                                (IDX6       >   50)
                PERFORM  4801-NYUKAIKEI-ERASE-SUB-SEC
           END-PERFORM
      *
           .
       4801-NYUKAIKEI-ERASE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院取消　会計テーブル編集処理２
      *****************************************************************
       4801-NYUKAIKEI-ERASE-SUB-SEC         SECTION.
      *
           INITIALIZE            ORCSNYUACCT-AREA
      *
      *---------------退院日の同日入院処理ここから----------------------
           MOVE   SPACE                TO  WRK-DOU-TENNYUYMD
      *    同日再入院用に入院日（転入日）を退避
           MOVE   SPA-NAI-I01-NYUINYMD TO  WRK-DOU-NAI-I01-NYUINYMD
      *    入院履歴の退避
           MOVE   PTNYUINRRK-REC       TO  WRKRRK-REC
      *    最終入院履歴が自院歴かを判定する
           MOVE    ZERO                TO  FLG-JIINRRK
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               IF      PTNYUINRRK-JTIKBN   NOT =   "5"  AND "6"
                   IF  PTNYUINRRK-TAIINYMD  = WRK-DOU-NAI-I01-NYUINYMD                   
                       MOVE   1        TO  FLG-JIINRRK
                   END-IF
               END-IF
           END-IF
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *    入院履歴の復元
           MOVE    WRKRRK-REC          TO  PTNYUINRRK-REC
      *    前歴が自院歴（同日退院分）の場合、入院会計作成日を１日ずらす
           MOVE    SPACE               TO  WRK-DOUJITSU-FLG
                                           WRK-DOUJITSU-MOTOYMD
           IF     FLG-JIINRRK          =   1
      *        同日再入院分の診療会計削除用引数追加対応
               IF   SPA-GMN-I01-SYORIKBN-CD(2:1)  =   "6"
                   MOVE   "1"          TO  LNK-SCNYUACCT-DOUJITSUKBN
                   MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  LNK-SCNYUACCT-SRYDELYMD
               END-IF
      *
               MOVE    "1"             TO  WRK-DOUJITSU-FLG
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  LNK-DAY6-KIJUN
                                           WRK-DOUJITSU-MOTOYMD
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    1               TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING     STS-AREA-DAY
                                                 LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  SPA-NAI-I01-NYUINYMD
           END-IF
      *---------------退院日の同日入院処理ここまで----------------------
      *
      *    入院会計作成サブの呼出し
           MOVE     SPA-GMN-I01-SYORIKBN-CD(2:1)
                                       TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     WRK-TENKA-SRYKA(IDX6)
                                       TO  SPA-SRYKA
           MOVE     SPA-NAI-I01-NYUINYMD
                                       TO   LNK-SCNYUACCT-STSRYYMD
      *    保険組合せ
           MOVE   PTNYUINRRK-HKNCOMBINUM
                                        TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9      TO  LNK-SCNYUACCT-HKNCOMBINUM
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
      *    入院カレンダー未設定の判定
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
           MOVE   PTNYUINRRK-BTUNUM   TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
                  DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                            LNK-SCNYUACCT-RC
           END-IF
      *
      *-------入院時食事時間区分対応ここから-----------------------------------
      *     DISPLAY "NYUIN-TORIKESI"
      *     DISPLAY "BAKRRK-DOUJITSUKBN   = " BAKRRK-DOUJITSUKBN
      *     DISPLAY "BAKRRK-NYUINYMD      = " BAKRRK-NYUINYMD
      *     DISPLAY "BAKRRK-TENNYUYMD     = " BAKRRK-TENNYUYMD
      *     DISPLAY "BAKRRK-TIMEKBN-IN    = " BAKRRK-TIMEKBN-IN
      *     DISPLAY "WRK-DOUJITSU-MOTOYMD = " WRK-DOUJITSU-MOTOYMD
      *     DISPLAY "WRK-DOUJITSU-FLG     = " WRK-DOUJITSU-FLG
           IF    WRK-DOUJITSU-FLG       =   "1"
      *
      *        朝食から
               IF  BAKRRK-TIMEKBN-IN    =    "1"
      *            朝食をクリア
                   MOVE    BAKRRK-HOSPNUM  TO  NACCT-HOSPNUM
                   MOVE    BAKRRK-PTID     TO  NACCT-PTID
                   MOVE    BAKRRK-NYUINYMD(1:6)
                                           TO  NACCT-SRYYM
                   MOVE    "key30"         TO  MCP-PATHNAME
                   PERFORM  910-NYUINACCT-SELECT-SEC
                   MOVE    "key30"         TO  MCP-PATHNAME
                   PERFORM  910-NYUINACCT-READ-SEC
                   IF      FLG-NYUINACCT        =   ZERO
                       MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                       MOVE     BAKRRK-NYUINYMD(7:2)
                                          TO  IDX-SHOKUJIX
                       MOVE     ZERO   TO  NACCT-DAY(IDX-SHOKUJI9)
      *                剤回数再セット
                       MOVE  ZERO              TO  NACCT-ZAIKAISU
                       PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL     IDX-SKJ   >   31
                            IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1        TO  NACCT-ZAIKAISU
                            END-IF
                       END-PERFORM   
                       MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                       MOVE     "tbl_nyuinacct" TO MCP-TABLE
                       MOVE     "key"          TO  MCP-PATHNAME
                       PERFORM  910-DBUPDATE-SEC
      *                入院会計マスタクローズ
                       MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                       MOVE    "key30"     TO  MCP-PATHNAME
                       PERFORM  910-DBCLOSECURSOR-SEC
                   END-IF
      *            昼食をクリア
                   MOVE    BAKRRK-HOSPNUM  TO  NACCT-HOSPNUM
                   MOVE    BAKRRK-PTID     TO  NACCT-PTID
                   MOVE    BAKRRK-NYUINYMD(1:6)
                                           TO  NACCT-SRYYM
                   MOVE    "key31"         TO  MCP-PATHNAME
                   PERFORM  910-NYUINACCT-SELECT-SEC
                   MOVE    "key31"         TO  MCP-PATHNAME
                   PERFORM  910-NYUINACCT-READ-SEC
                   IF      FLG-NYUINACCT        =   ZERO
                       MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                       MOVE     BAKRRK-NYUINYMD(7:2)
                                          TO  IDX-SHOKUJIX
                       MOVE     ZERO   TO  NACCT-DAY(IDX-SHOKUJI9)
      *                剤回数再セット
                       MOVE  ZERO              TO  NACCT-ZAIKAISU
                       PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL     IDX-SKJ   >   31
                            IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1        TO  NACCT-ZAIKAISU
                            END-IF
                       END-PERFORM   
                       MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                       MOVE     "tbl_nyuinacct" TO MCP-TABLE
                       MOVE     "key"          TO  MCP-PATHNAME
                       PERFORM  910-DBUPDATE-SEC
      *                入院会計マスタクローズ
                       MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                       MOVE    "key31"     TO  MCP-PATHNAME
                       PERFORM  910-DBCLOSECURSOR-SEC
                   END-IF
      *            夕食をクリア
                   MOVE    BAKRRK-HOSPNUM  TO  NACCT-HOSPNUM
                   MOVE    BAKRRK-PTID     TO  NACCT-PTID
                   MOVE    BAKRRK-NYUINYMD(1:6)
                                           TO  NACCT-SRYYM
                   MOVE    "key32"         TO  MCP-PATHNAME
                   PERFORM  910-NYUINACCT-SELECT-SEC
                   MOVE    "key32"         TO  MCP-PATHNAME
                   PERFORM  910-NYUINACCT-READ-SEC
                   IF      FLG-NYUINACCT        =   ZERO
                       MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                       MOVE     BAKRRK-NYUINYMD(7:2)
                                          TO  IDX-SHOKUJIX
                       MOVE     ZERO   TO  NACCT-DAY(IDX-SHOKUJI9)
      *                剤回数再セット
                       MOVE  ZERO              TO  NACCT-ZAIKAISU
                       PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL     IDX-SKJ   >   31
                            IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1        TO  NACCT-ZAIKAISU
                            END-IF
                       END-PERFORM   
                       MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                       MOVE     "tbl_nyuinacct" TO MCP-TABLE
                       MOVE     "key"          TO  MCP-PATHNAME
                       PERFORM  910-DBUPDATE-SEC
      *                入院会計マスタクローズ
                       MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                       MOVE    "key32"     TO  MCP-PATHNAME
                       PERFORM  910-DBCLOSECURSOR-SEC
                   END-IF
               END-IF
      *
      *        昼食から
               IF  BAKRRK-TIMEKBN-IN    =    "2"
      *            昼食をクリア
                   MOVE    BAKRRK-HOSPNUM  TO  NACCT-HOSPNUM
                   MOVE    BAKRRK-PTID     TO  NACCT-PTID
                   MOVE    BAKRRK-NYUINYMD(1:6)
                                           TO  NACCT-SRYYM
                   MOVE    "key31"         TO  MCP-PATHNAME
                   PERFORM  910-NYUINACCT-SELECT-SEC
                   MOVE    "key31"         TO  MCP-PATHNAME
                   PERFORM  910-NYUINACCT-READ-SEC
                   IF      FLG-NYUINACCT        =   ZERO
                       MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                       MOVE     BAKRRK-NYUINYMD(7:2)
                                          TO  IDX-SHOKUJIX
                       MOVE     ZERO   TO  NACCT-DAY(IDX-SHOKUJI9)
      *                剤回数再セット
                       MOVE  ZERO              TO  NACCT-ZAIKAISU
                       PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL     IDX-SKJ   >   31
                            IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1        TO  NACCT-ZAIKAISU
                            END-IF
                       END-PERFORM   
                       MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                       MOVE     "tbl_nyuinacct" TO MCP-TABLE
                       MOVE     "key"          TO  MCP-PATHNAME
                       PERFORM  910-DBUPDATE-SEC
      *                入院会計マスタクローズ
                       MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                       MOVE    "key31"     TO  MCP-PATHNAME
                       PERFORM  910-DBCLOSECURSOR-SEC
                   END-IF
      *            夕食をクリア
                   MOVE    BAKRRK-HOSPNUM  TO  NACCT-HOSPNUM
                   MOVE    BAKRRK-PTID     TO  NACCT-PTID
                   MOVE    BAKRRK-NYUINYMD(1:6)
                                           TO  NACCT-SRYYM
                   MOVE    "key32"         TO  MCP-PATHNAME
                   PERFORM  910-NYUINACCT-SELECT-SEC
                   MOVE    "key32"         TO  MCP-PATHNAME
                   PERFORM  910-NYUINACCT-READ-SEC
                   IF      FLG-NYUINACCT        =   ZERO
                       MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                       MOVE     BAKRRK-NYUINYMD(7:2)
                                          TO  IDX-SHOKUJIX
                       MOVE     ZERO   TO  NACCT-DAY(IDX-SHOKUJI9)
      *                剤回数再セット
                       MOVE  ZERO              TO  NACCT-ZAIKAISU
                       PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL     IDX-SKJ   >   31
                            IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1        TO  NACCT-ZAIKAISU
                            END-IF
                       END-PERFORM   
                       MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                       MOVE     "tbl_nyuinacct" TO MCP-TABLE
                       MOVE     "key"          TO  MCP-PATHNAME
                       PERFORM  910-DBUPDATE-SEC
      *                入院会計マスタクローズ
                       MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                       MOVE    "key32"     TO  MCP-PATHNAME
                       PERFORM  910-DBCLOSECURSOR-SEC
                   END-IF
               END-IF
      *
      *        夕食から
               IF  BAKRRK-TIMEKBN-IN    =    "3"
      *            夕食をクリア
                   MOVE    BAKRRK-HOSPNUM  TO  NACCT-HOSPNUM
                   MOVE    BAKRRK-PTID     TO  NACCT-PTID
                   MOVE    BAKRRK-NYUINYMD(1:6)
                                           TO  NACCT-SRYYM
                   MOVE    "key32"         TO  MCP-PATHNAME
                   PERFORM  910-NYUINACCT-SELECT-SEC
                   MOVE    "key32"         TO  MCP-PATHNAME
                   PERFORM  910-NYUINACCT-READ-SEC
                   IF      FLG-NYUINACCT        =   ZERO
                       MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                       MOVE     BAKRRK-NYUINYMD(7:2)
                                          TO  IDX-SHOKUJIX
                       MOVE     ZERO   TO  NACCT-DAY(IDX-SHOKUJI9)
      *                剤回数再セット
                       MOVE  ZERO              TO  NACCT-ZAIKAISU
                       PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL     IDX-SKJ   >   31
                            IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1        TO  NACCT-ZAIKAISU
                            END-IF
                       END-PERFORM   
                       MOVE     NYUINACCT-REC  TO  MCPDATA-REC
                       MOVE     "tbl_nyuinacct" TO MCP-TABLE
                       MOVE     "key"          TO  MCP-PATHNAME
                       PERFORM  910-DBUPDATE-SEC
      *                入院会計マスタクローズ
                       MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                       MOVE    "key32"     TO  MCP-PATHNAME
                       PERFORM  910-DBCLOSECURSOR-SEC
                   END-IF
               END-IF
      *
           END-IF
      *
      *-------入院時食事時間区分対応ここまで-----------------------------------
      *
      *-------入院取消日の食堂加算対応ここから---------------------------------
           IF    WRK-DOUJITSU-FLG       =   "1"
      *
             MOVE    ZERO                TO  WRK-SHOKUJI-ASA
             MOVE    ZERO                TO  WRK-SHOKUJI-HIRU
             MOVE    ZERO                TO  WRK-SHOKUJI-YORU
      *      朝食
             MOVE    BAKRRK-HOSPNUM      TO  NACCT-HOSPNUM
             MOVE    BAKRRK-PTID         TO  NACCT-PTID
             MOVE    BAKRRK-NYUINYMD(1:6)
                                         TO  NACCT-SRYYM
             MOVE    "key30"             TO  MCP-PATHNAME
             PERFORM  910-NYUINACCT-SELECT-SEC
             MOVE    "key30"             TO  MCP-PATHNAME
             PERFORM  910-NYUINACCT-READ-SEC
             IF      FLG-NYUINACCT        =   ZERO
                 MOVE     BAKRRK-NYUINYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 IF   NACCT-DAY(IDX-SHOKUJI9)  =  1  OR  2  OR  3
                     MOVE    1           TO  WRK-SHOKUJI-ASA
                 END-IF
             END-IF
      *      入院会計マスタクローズ
             MOVE    "tbl_nyuinacct" TO  MCP-TABLE
             MOVE    "key30"     TO  MCP-PATHNAME
             PERFORM  910-DBCLOSECURSOR-SEC
      *
      *      昼食
             MOVE    BAKRRK-HOSPNUM      TO  NACCT-HOSPNUM
             MOVE    BAKRRK-PTID         TO  NACCT-PTID
             MOVE    BAKRRK-NYUINYMD(1:6)
                                         TO  NACCT-SRYYM
             MOVE    "key31"             TO  MCP-PATHNAME
             PERFORM  910-NYUINACCT-SELECT-SEC
             MOVE    "key31"             TO  MCP-PATHNAME
             PERFORM  910-NYUINACCT-READ-SEC
             IF      FLG-NYUINACCT        =   ZERO
                 MOVE     BAKRRK-NYUINYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 IF   NACCT-DAY(IDX-SHOKUJI9)  =  1  OR  2  OR  3
                     MOVE    1           TO  WRK-SHOKUJI-HIRU
                 END-IF
             END-IF
      *      入院会計マスタクローズ
             MOVE    "tbl_nyuinacct" TO  MCP-TABLE
             MOVE    "key31"     TO  MCP-PATHNAME
             PERFORM  910-DBCLOSECURSOR-SEC
      *
      *      夕食
             MOVE    BAKRRK-HOSPNUM      TO  NACCT-HOSPNUM
             MOVE    BAKRRK-PTID         TO  NACCT-PTID
             MOVE    BAKRRK-NYUINYMD(1:6)
                                         TO  NACCT-SRYYM
             MOVE    "key32"             TO  MCP-PATHNAME
             PERFORM  910-NYUINACCT-SELECT-SEC
             MOVE    "key32"             TO  MCP-PATHNAME
             PERFORM  910-NYUINACCT-READ-SEC
             IF      FLG-NYUINACCT        =   ZERO
                 MOVE     BAKRRK-NYUINYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 IF   NACCT-DAY(IDX-SHOKUJI9)  =  1  OR  2  OR  3
                     MOVE    1           TO  WRK-SHOKUJI-YORU
                 END-IF
             END-IF
      *      入院会計マスタクローズ
             MOVE    "tbl_nyuinacct" TO  MCP-TABLE
             MOVE    "key32"     TO  MCP-PATHNAME
             PERFORM  910-DBCLOSECURSOR-SEC
      *
      *      食堂加算の再セット
             IF    (WRK-SHOKUJI-ASA      =   ZERO)   AND
                   (WRK-SHOKUJI-HIRU     =   ZERO)   AND
                   (WRK-SHOKUJI-YORU     =   ZERO)
                 MOVE    BAKRRK-HOSPNUM      TO  NACCT-HOSPNUM
                 MOVE    BAKRRK-PTID         TO  NACCT-PTID
                 MOVE    BAKRRK-NYUINYMD(1:6)
                                             TO  NACCT-SRYYM
                 MOVE    "key43"             TO  MCP-PATHNAME
                 PERFORM  910-NYUINACCT-SELECT-SEC
                 MOVE    "key43"             TO  MCP-PATHNAME
                 PERFORM  910-NYUINACCT-READ-SEC
                 IF      FLG-NYUINACCT        =   ZERO
                     MOVE     BAKRRK-NYUINYMD(7:2)
                                             TO  IDX-SHOKUJIX
                     MOVE    ZERO            TO  NACCT-DAY(IDX-SHOKUJI9)
      *              剤回数再セット
                     MOVE  ZERO              TO  NACCT-ZAIKAISU
                     PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                       UNTIL     IDX-SKJ   >   31
                          IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1       TO  NACCT-ZAIKAISU
                          END-IF
                     END-PERFORM
                     MOVE     NYUINACCT-REC   TO  MCPDATA-REC
                     MOVE     "tbl_nyuinacct" TO MCP-TABLE
                     MOVE     "key"           TO  MCP-PATHNAME
                     PERFORM  910-DBUPDATE-SEC
                 END-IF
      *          入院会計マスタクローズ
                 MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                 MOVE    "key43"     TO  MCP-PATHNAME
                 PERFORM  910-DBCLOSECURSOR-SEC
             END-IF
      *
           END-IF
      *-------入院取消日の食堂加算対応ここまで---------------------------------
      *
           .
       4801-NYUKAIKEI-ERASE-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院取消　会計テーブル編集処理
      *****************************************************************
       4801-TAIINKAIKEI-CANCEL-SEC         SECTION.
      *
           MOVE      SPACE        TO    WRK-DOU-TENNYUYMD
           MOVE      SPACE        TO    IPN-NYUKAIKEI-SAKUSEI-FLG
      *
      *    特定集中治療室管理料算定時に退院日をＳＰＡ−異動日と転入日にセット
      *    以下の特定入院料も同様
      *      救命救急入院料１
      *      救命救急入院料２
      *      一類感染症患者入院医療管理料
      *      広範囲熱傷特定集中治療室管理料
      *      ハイケアユニット入院医療管理料
      *      亜急性期入院医療管理料
      *      脳卒中ケアユニット入院医療管理料
      *      亜急性期入院医療管理料２
      *      亜急性期入院医療管理料１（指定地域）
      *      亜急性期入院医療管理料２（指定地域）
      *      救命救急入院料３（救命救急入院料）
      *      救命救急入院料３（広範囲熱傷特定集中治療）
      *      救命救急入院料４（救命救急入院料）
      *      救命救急入院料４（広範囲熱傷特定集中治療）
      *      特定集中治療室管理料２（特定集中治療室管理料）
      *      特定集中治療室管理料２（広範囲熱傷特定集中治療）
      *      小児特定集中治療室管理料
      *      特定一般病棟入院料（亜急性期入院医療管理）
      *      特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
      *      特定集中治療室管理料１（平成２６年改正）
      *      特定集中治療室管理料２（特定集中治療室管理料）（平成２６年改正）
      *      特定集中治療室管理料２（広範囲熱傷特定集中治療）（平成２６年改正）
      *      ハイケアユニット入院医療管理料１（平成２６年改正）
      *      ハイケアユニット入院医療管理料２（平成２６年改正）
      *      地域包括ケア入院医療管理料１（平成２６年改正）
      *      地域包括ケア入院医療管理料１（生活療養）（平成２６年改正）
      *      地域包括ケア入院医療管理料２（平成２６年改正）
      *      地域包括ケア入院医療管理料２（生活療養）（平成２６年改正）
      *      地域包括ケア入院医療管理料１（特定地域）（平成２６年改正）
      *      地域包括ケア入院医療管理料１（生活療養）（特定地域）（平成２６年改正）
      *      地域包括ケア入院医療管理料２（特定地域）（平成２６年改正）
      *      地域包括ケア入院医療管理料２（生活療養）（特定地域）（平成２６年改正）
      *      特定一般病棟入院料（地域包括ケア入院医療管理１）（平成２６年改正）
      *      特定一般病棟入院料（地域包括ケア入院医療管理２）（平成２６年改正）
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN  = "03" OR "04" OR "05" OR
                                                "06" OR "07" OR "08" OR
                                                "09" OR "13" OR "14" OR
                                                "16" OR "17" OR "18" OR
                                                "19" OR "54" OR "55" OR
                                                "56" OR "57" OR "58" OR
                                                "59" OR "60" OR "91" OR
                                                "92" OR "93" OR "95" OR
                                                "96" OR "AH" OR "AI" OR
                                                "AT" OR "AJ" OR "AK" OR
                                                "AL" OR "AM" OR "AN" OR
                                                "AO" OR "AP" OR "AQ" OR
                                                "AR" OR "AS" OR "AU" OR
                                                "AV"
                    MOVE   SPA-NAI-I01-TAIINYMD
                                       TO   PTNYUINRRK-TENNYUYMD
                                            SPA-NAI-I01-IDOYMD

               END-IF
      *        療養病棟と療養病床も判定条件に追加
               IF    ((PTNYUINRRK-TOKUTEI-NYUIN >=   "25")  AND
                      (PTNYUINRRK-TOKUTEI-NYUIN <=   "48"))
                                             OR
                     ((PTNYUINRRK-TOKUTEI-NYUIN >=   "63")  AND
                      (PTNYUINRRK-TOKUTEI-NYUIN <=   "88"))
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    SPA-NAI-I01-TAIINYMD
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO  PTNYUINRRK-TENNYUYMD
               END-IF
           ELSE
               IF  PTNYUINRRK-TOKUTEI-KBN   =   SPACE
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    SPA-NAI-I01-TAIINYMD
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO  PTNYUINRRK-TENNYUYMD
      *             MOVE   SPA-NAI-I01-TAIINYMD
      *                                 TO   PTNYUINRRK-TENNYUYMD
               END-IF
      *        緩和ケア病棟入院料、認知症治療病棟入院料１、認知症治療病棟入院料２
      *        を算定時に退院日をＳＰＡ−異動日と転入日にセット
      *        以下の特定入院料も同様
      *        地域包括ケア病棟入院料１（平成２６年改正）
      *        地域包括ケア病棟入院料１（生活療養）（平成２６年改正）
      *        地域包括ケア病棟入院料２（平成２６年改正）
      *        地域包括ケア病棟入院料２（生活療養）（平成２６年改正）
      *        地域包括ケア病棟入院料１（特定地域）（平成２６年改正）
      *        地域包括ケア病棟入院料１（生活療養）（特定地域）（平成２６年改正）
      *        地域包括ケア病棟入院料２（特定地域）（平成２６年改正）
      *        地域包括ケア病棟入院料２（生活療養）（特定地域）（平成２６年改正）
               IF   (PTNYUINRRK-TOKUTEI-KBN   =   "2")  AND
                    (PTNYUINRRK-TOKUTEI-NYUIN  = "09" OR "05" OR "15"
                                              OR "41" OR "42" OR "43"
                                              OR "44" OR "45" OR "46"
                                              OR "47" OR "48" )
                    MOVE   SPA-NAI-I01-TAIINYMD
                                       TO   PTNYUINRRK-TENNYUYMD
                                            SPA-NAI-I01-IDOYMD
               END-IF
           END-IF
      *
      *    退院取消前の会計元データ作成
           INITIALIZE      ORCSSYOKIKSNAREA
      *
      *    入院会計削除前に会計データの食事区分と保険組合せを退避
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    "00"                TO  NACCT-SRYKA
           MOVE   SPA-NAI-I01-TAIINYMD(1:6)
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO        TO      WRK-TAIHI-SHOKUJI
               PERFORM UNTIL       MCP-RC       NOT =   ZERO
      *
                   MOVE     MCPDATA-REC   TO   NYUINACCT-REC
      *
      *            食事を退避
                   IF     (NACCT-ZAISKBKBN     =   "4")   AND
                          (NACCT-SRYCDTOTAL    =   99999920)
                       MOVE    SPA-NAI-I01-TAIINYMD(7:2)
                                         TO    IDX-SHOKUJIX
                       IF      NACCT-DAY(IDX-SHOKUJI9)  NOT =   ZERO
                            MOVE   NACCT-DAY(IDX-SHOKUJI9) TO
                                                  WRK-TAIHI-SHOKUJI
                       END-IF
                   END-IF
      *            保険を退避
                   IF      NACCT-ZAISKBKBN     =   "5"
                       MOVE   SPA-NAI-I01-TAIINYMD(7:2)
                                         TO    IDX-SHOKUJIX
                       IF      NACCT-DAY(IDX-SHOKUJI9)  NOT =   ZERO
                            MOVE   NACCT-DAY(IDX-SHOKUJI9) TO
                                                  WRK-TAIHI-HKN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key16"             TO  MCP-PATHNAME
                   PERFORM 910-DBFETCH-SEC
               END-PERFORM
           ELSE
               INITIALIZE                  NYUINACCT-REC
           END-IF
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *
      *     DISPLAY "=========KAKUNIN =================="
           MOVE     SPA-NAI-I01-TAIINYMD  TO  PTNYUINRRK-TENNYUYMD
      *     DISPLAY "WRK-DOU-TENNYUYMD = " WRK-DOU-TENNYUYMD
      *     DISPLAY "PTNYUINRRK-TENNYUYMD = " PTNYUINRRK-TENNYUYMD
      *    通算日数、算定開始日を求める
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( SPA-ERRCD    NOT =  SPACE )
               GO  TO  4801-TAIINKAIKEI-CANCEL-EXT
           END-IF
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    WRK-SANTEISTYMD
                                   TO  SYOKIKSN-IN-SANTEISTYMD
      *
      *     DISPLAY "====AZU  TAIIN-CANCEL=================="
      *     DISPLAY "RRKNUM     = " PTNYUINRRK-RRKNUM
      *     DISPLAY "RRKEDANUM  = " PTNYUINRRK-RRKEDANUM
      *     DISPLAY "NYUINYMD   = " PTNYUINRRK-NYUINYMD
      *     DISPLAY "TAIINYMD   = " PTNYUINRRK-TAIINYMD
      *     DISPLAY "TENNYUYMD  = " PTNYUINRRK-TENNYUYMD
      *     DISPLAY "TENSTUYMD  = " PTNYUINRRK-TENSTUYMD
      *
      *    平成２４年３月３１日時点で亜急性期入院医療管理料１を算定
      *    している状態か判定するサブを呼び出し
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "18"  OR  "54"
                   INITIALIZE                  ORCSAKYUSEI-AREA
                   MOVE    PTNYUINRRK-HOSPNUM  TO  LNK-AKYUSEI-HOSPNUM
                   MOVE    PTNYUINRRK-PTID     TO  LNK-AKYUSEI-PTID
                   MOVE    PTNYUINRRK-RRKNUM   TO  LNK-AKYUSEI-RRKNUM
                   CALL    "ORCSAKYUSEI"    USING  ORCSAKYUSEI-AREA
                                                   SPA-AREA
                   DISPLAY  "LNK-AKYUSEI-RC = " LNK-AKYUSEI-RC
               END-IF
           END-IF
      *
           PERFORM  4801-BTUSEL-SEC
           MOVE     SYS-5001-BTU-KHNSRYCD
                          TO    SYOKIKSN-IN-KHNSRYCD
      *
      *
           IF      SPA-GMN-I01-TOKNYUIN-CD    NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
      *        特定一般病棟入院料の場合は期間加算が存在するため、
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        療養病床で有床診療所入院基本料を相互算定する場合も
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        特定入院料の算定要件外に算定にする以下入院料を追加（平成２６年改正で対応）
      *        ・一般病棟特別入院基本料
      *        ・精神病棟１５対１入院基本料
      *        ・精神病棟特別入院基本料
      *        以下入院料を追加（平成２８年改正で対応）
      *        ・療養病棟・一般病棟７対１入院基本料
      *        ・療養病棟・一般病棟１０対１入院基本料
      *        ・療養病棟・一般病棟１３対１入院基本料
      *        ・療養病棟・一般病棟１５対１入院基本料
      *        以下入院料を追加（平成３０年改正で対応）
      *        ・療養病棟・急性期一般入院料２
      *        ・療養病棟・急性期一般入院料３
      *        ・療養病棟・急性期一般入院料４
      *        ・療養病棟・急性期一般入院料５
      *        ・療養病棟・急性期一般入院料６
      *        ・療養病棟・地域一般入院料１
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"   OR
                                                "099999955"   OR
                                                "099999956"   OR
                                                "099999957"   OR
      *                                         特定入院料の算定要件外の算定
                                                "190079010"   OR
                                                "190083810"   OR
                                                "190085010"   OR
      *                                         平成２８年改正対応
                                                "190198110"   OR
                                                "190198210"   OR
                                                "190198310"   OR
                                                "190198410"   OR
      *                                         平成３０年改正対応
                                                "190201910"   OR
                                                "190202010"   OR
                                                "190202110"   OR
                                                "190202210"   OR
                                                "190202310"   OR
                                                "190202410"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4017"      TO  SPA-ERRCD
                       GO  TO  4801-TAIINKAIKEI-CANCEL-EXT
                   END-IF
               END-IF
           ELSE
               IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM   4801-RJNCD-CHG-SEC
                   PERFORM   4801-YAKINGEN-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4019"      TO  SPA-ERRCD
                       GO  TO  4801-TAIINKAIKEI-CANCEL-EXT
                   END-IF
               ELSE
                   MOVE    "4020"      TO  SPA-ERRCD
                   GO  TO  4801-TAIINKAIKEI-CANCEL-EXT
               END-IF
           END-IF
      *
      *    入院会計作成サブの呼出し
           INITIALIZE                      ORCSNYUACCT-AREA
           MOVE     "7"                TO  LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE     SPA-NAI-I01-NYUINYMD
                                       TO   LNK-SCNYUACCT-STSRYYMD
           MOVE     SPA-NAI-I01-TAIINYMD
                                       TO   LNK-SCNYUACCT-EDSRYYMD
           MOVE    SYOKIKSN-IN-KHNSRYCD
                            TO   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *退院日からの会計再作成に変更 ---------------------------------
      **     MOVE    SYOKIKSN-IN-SANTEISTYMD
      **                      TO   LNK-SCNYUACCT-STSRYYMD
           MOVE     SPA-NAI-I01-TAIINYMD
                                       TO   LNK-SCNYUACCT-STSRYYMD
      *--------------------------------------------------------------
      *
           PERFORM   VARYING   IDX6   FROM   1   BY   1
               UNTIL (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE) OR
                     (IDX6   >     7)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX6)
           END-PERFORM
      *
      *    通算算定済日数
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    特定入院で入院料が変更になるコードの対応
      *
      *     DISPLAY "===  TEST TAIINKAIKEI-CANCEL ==="
           INITIALIZE                       ORCSTOKNYUIN2-AREA
           MOVE   PTNYUINRRK-HOSPNUM    TO  LNK-TOKNYUIN2-HOSPNUM
           MOVE   PTNYUINRRK-PTID       TO  LNK-TOKNYUIN2-PTID
           MOVE   "1"                   TO  LNK-TOKNYUIN2-IDOKBN
           MOVE   PTNYUINRRK-NYUINYMD   TO  LNK-TOKNYUIN2-NYUINYMD
           MOVE   SPA-NAI-I01-TAIINYMD  TO  LNK-TOKNYUIN2-SANTEIYMD
           MOVE   SYOKIKSN-IN-KHNSRYCD  TO  LNK-TOKNYUIN2-SET-SRYCD
           MOVE   STUSAN-NISSU08        TO  LNK-TOKNYUIN2-NISSU08
           MOVE   PTNYUINRRK-SHONUM     TO  LNK-TOKNYUIN2-SHONUM
           MOVE   PTNYUINRRK-TOKUTEI-NYUIN
                             TO  LNK-TOKNYUIN2-TOKUTEI-NYUIN
           IF    (LNK-AKYUSEI-RC        =   1)           AND
                 (SPA-NAI-I01-TAIINYMD  >=   "20120401")
              MOVE   "1"      TO   LNK-TOKNYUIN2-TOKUBETSU-FLG(1)
           END-IF
      *    療養病棟（病床）入院のフラグセット
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-5001-KBNCD
           MOVE    SPA-NAI-I01-TAIINYMD
                                       TO  SYS-5001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD (7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
               IF    SYS-5001-BTU-SBT    =   "04"  OR  "09"
                       MOVE    "1"     TO
                                         LNK-TOKNYUIN2-TOKUBETSU-FLG(2)
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CALL  "ORCSTOKNYUIN2"     USING  ORCSTOKNYUIN2-AREA
                                            SPA-AREA
           IF     LNK-TOKNYUIN2-RC      =   ZERO
              MOVE   SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
              PERFORM   VARYING   IDX    FROM    1   BY  1
                UNTIL   IDX       >    5
                  IF   LNK-TOKNYUIN2-SRYCD(IDX)  NOT =   SPACE
                    MOVE  LNK-TOKNYUIN2-SRYCD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                    MOVE  LNK-TOKNYUIN2-STYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX)
                    MOVE  LNK-TOKNYUIN2-EDYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX)
                  END-IF
              END-PERFORM
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                          TO  IPN-NYUKAIKEI-SAKUSEI-FLG
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                          TO  IPN-SANTEISTYMD
           ELSE
              IF      PTNYUINRRK-TOKUTEI-NYUIN  NOT =   SPACE
                IF    LNK-TOKNYUIN2-RC  =   1
                    MOVE    SPACE         TO  SPA-GMN-I01-TOKNYUIN-CD
                                              SYOKIKSN-IN-KHNSRYCD
                                          LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                END-IF
                MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                          TO  IPN-NYUKAIKEI-SAKUSEI-FLG
                MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                          TO  IPN-SANTEISTYMD
              END-IF
           END-IF
      *
      *
      *-------特定入院対応ここまで-----------------------------------
      *
      *    入院料加算診療コード取り込み
           PERFORM    4801-NYUKSN-GET-SEC
      *
      *    退院日に救急・在宅等支援療養病床初期加算（病院）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
           PERFORM   210-KYUKYU-KSN1-SEC
      *
      *    退院日に救急・在宅等支援療養病床初期加算（診療所）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
           PERFORM   210-KYUKYU-KSN2-SEC
      *
      *    退院日に救急・在宅等支援療養病床初期加算（療養病棟入院基本料１）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
           PERFORM   210-KYUKYU-KSN3-SEC
      *
      *    退院日に救急・在宅等支援療養病床初期加算（一般病棟入院基本料）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
           PERFORM   210-KYUKYU-KSN4-SEC
      *
      *    退院日に救急・在宅等支援療養病床初期加算（特定一般病棟入院料）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
           PERFORM   210-KYUKYU-KSN5-SEC
      *
      *    退院日に救急・在宅等支援療養病床初期加算（地域包括ケア病棟入院料）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
           PERFORM   210-KYUKYU-KSN6-SEC
      *
      *    退院日に有床診療所一般病床初期加算が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
           PERFORM   210-YUUSHOUIPPAN-KSN-SEC
      *
      *    退院日に在宅患者支援療養病床初期加算（療養病棟入院基本料）が算定
      *    されているかチェックして、算定されていれば入院料加算の扱いに含める
           PERFORM   210-ZAITAKU-KSN1-SEC
      *
      *    退院日に在宅患者支援病床初期加算（地域包括ケア病棟入院料）が算定
      *    されているかチェックして、算定されていれば入院料加算の扱いに含める
           PERFORM   210-ZAITAKU-KSN2-SEC
      *
      *    特定入院料区分のセット
           IF      SPA-GMN-I01-TOKNYUIN-CD    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        特定入院基本料、療養病棟入院基本料、療養病床入院基本料等は
      *        特定入院料扱いから外す
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
      *            特定入院基本料（一般病棟入院基本料）はマスタが
      *            平成２６年９月３０日までの為、参照時の日付を変更
                   IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                       MOVE    "20140930"   TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   END-IF
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
               IF    LNK-SCNYUACCT-KAGEN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *    外泊
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9
                                TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    食事区分
           MOVE   "099999913"   TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           MOVE   WRK-TAIHI-SHOKUJI
                                TO  LNK-SCNYUACCT-SHOKUJIKBN
           IF     PTNYUINRRK-SKJKBN    =   "B"  OR  "C"
               MOVE    "1"             TO  LNK-SCNYUACCT-SHOKUJI-SETKBN
               MOVE   PTNYUINRRK-SKJKBN-ASA
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-ASA
               MOVE   PTNYUINRRK-SKJKBN-HIRU
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-HIRU
               MOVE   PTNYUINRRK-SKJKBN-YU
                                       TO  LNK-SCNYUACCT-SHOKUJIKBN-YORU
           END-IF
      *    食堂加算
           PERFORM     4801-BTUSEL-SEC
           IF   FLG-SYSKANRI           =    ZERO
               IF   SYS-5001-BTU-SYOKUDOKBN       =    "2"
                    MOVE     "197000570"    TO
                                LNK-SCNYUACCT-SHOKUDO-SRYCD
                    MOVE    1   TO   LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    保険組合せ
           MOVE   "099999914"   TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           IF     WRK-TAIHI-HKN    NOT =   ZERO
                  MOVE   WRK-TAIHI-HKN
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           ELSE
                  MOVE   PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
                  MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           END-IF
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           IF     PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
      *    入院カレンダー未設定の判定
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え
           IF    (LNK-SCNYUACCT-STSRYYMD   >=   "20120401")  AND
                 (LNK-SCNYUACCT-STSRYYMD   <=   "20140331")
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え（平成２６年改定）
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20140401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169610"
                             MOVE "190170810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169710"
                             MOVE "190170910" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169810"
                             MOVE "190171010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169910"
                             MOVE "190171110" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170010"
                             MOVE "190171210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170110"
                             MOVE "190171310" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170210"
                             MOVE "190171410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170310"
                             MOVE "190171510" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170410"
                             MOVE "190171610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    特定入院基本料のコード細分化置き換え
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161010" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161210" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
      *
      *            特定機能病院は病棟設定された入院コードから判定
                   IF   SYS-5001-BTU-KHNSRYCD =  "190118610" OR
                                                 "190705610" OR
                                                 "190085910"
                     MOVE "190160810" TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-IF
               END-IF
      *        特別入院基本料
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799510"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161110" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161310" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE   PTNYUINRRK-BTUNUM   TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
                  DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                            LNK-SCNYUACCT-RC
           END-IF
      *
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
               MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
               MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
               MOVE    IPN-SANTEISTYMD   TO   SPA-NAI-I01-IDOYMD
               MOVE    IPN-SANTEISTYMD   TO   PTNYUINRRK-TENNYUYMD
               PERFORM   4801-TENROOM-EDIT-SEC
           END-IF
      *
           .
       4801-TAIINKAIKEI-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    救急・在宅等支援療養病床初期加算（病院）の算定有無チェック
      *****************************************************************
       210-KYUKYU-KSN1-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    転入日付のマイナス１日
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING        STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key44"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key44"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190134270"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key44"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-KYUKYU-KSN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    救急・在宅等支援療養病床初期加算（診療所）の算定有無チェック
      *****************************************************************
       210-KYUKYU-KSN2-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    転入日付のマイナス１日
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING    STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key45"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key45"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190135670"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key45"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-KYUKYU-KSN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    救急・在宅等支援療養病床初期加算（療養病棟入院基本料１）の
      *    算定有無チェック
      *****************************************************************
       210-KYUKYU-KSN3-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    転入日付のマイナス１日
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING        STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key48"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key48"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190143370"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key48"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-KYUKYU-KSN3-EXT.
           EXIT.
      *
      *****************************************************************
      *    救急・在宅等支援療養病床初期加算（一般病棟入院基本料）の
      *    算定有無チェック
      *****************************************************************
       210-KYUKYU-KSN4-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    転入日付のマイナス１日
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING        STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key49"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key49"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190142970"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key49"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-KYUKYU-KSN4-EXT.
           EXIT.
      *
      *****************************************************************
      *    救急・在宅等支援療養病床初期加算（特定一般病棟入院料）の
      *    算定有無チェック
      *****************************************************************
       210-KYUKYU-KSN5-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    転入日付のマイナス１日
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING        STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key50"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key50"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190152470"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key50"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-KYUKYU-KSN5-EXT.
           EXIT.
      *
      *****************************************************************
      *    救急・在宅等支援療養病床初期加算（地域包括ケア病棟入院料）の
      *    算定有無チェック
      *****************************************************************
       210-KYUKYU-KSN6-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    転入日付のマイナス１日
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING        STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key52"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key52"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190177170"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key52"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-KYUKYU-KSN6-EXT.
           EXIT.
      *
      *****************************************************************
      *    有床診療所一般病床初期加算の算定有無チェック
      *****************************************************************
       210-YUUSHOUIPPAN-KSN-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    転入日付のマイナス１日
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING    STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key46"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key46"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190135470"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key46"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-YUUSHOUIPPAN-KSN-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅患者支援療養病床初期加算（療養病棟入院基本料）の算定有無チェック
      *****************************************************************
       210-ZAITAKU-KSN1-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    転入日付のマイナス１日
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING        STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key60"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key60"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190201870"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key60"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-ZAITAKU-KSN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅患者支援病床初期加算（地域包括ケア病棟入院料）の算定有無チェック
      *****************************************************************
       210-ZAITAKU-KSN2-SEC            SECTION.
      *
           MOVE     ZERO               TO  WRK-TENNYUYMD9
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
      *    転入日付のマイナス１日
           INITIALIZE   LNK-DAY6-AREA
           MOVE  "61"         TO  LNK-DAY6-IRAI
           IF    FLG-IDOKAIKEI-CANCEL   =  0
               MOVE  PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
           ELSE
               MOVE  SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           END-IF
           MOVE  "1"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -1                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"    USING        STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE   LNK-DAY6-KEISAN      TO  WRK-TENNYUYMD
           MOVE   WRK-TENNYUYMD(1:6)   TO  NACCT-SRYYM
           MOVE    "key61"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    "key61"         TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      =    0
                    IF   NACCT-DAY(WRK-TENNYUYMD-DD9)  =  1
      **                   ADD    1      TO   IDX8
                         MOVE   "190212470"
                              TO   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key61"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-ZAITAKU-KSN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    転科／転棟／転室時　会計テーブル編集処理
      *****************************************************************
       4801-TENROOM-EDIT-SEC         SECTION.
      *
           MOVE      SPACE        TO    WRK-DOU-TENNYUYMD
           MOVE      SPACE        TO    IPN-NYUKAIKEI-SAKUSEI-FLG
      *
           INITIALIZE      ORCSSYOKIKSNAREA
      *
      *    通算日数、算定開始日を求める
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( SPA-ERRCD    NOT =  SPACE )
               GO  TO  4801-TENROOM-EDIT-EXT
           END-IF
      *
      *    介護病棟からの急性増悪による入院時
           IF      PTNYUINRRK-KAIGOKBN    =    "2"
               COMPUTE    WRK-TSUSAN   =   STUSAN-NISSU01  -  1
           END-IF
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
      *
           MOVE    WRK-SANTEISTYMD
                                   TO  SYOKIKSN-IN-SANTEISTYMD
      *
      *     DISPLAY "====AZU  TENROOM-EDIT =================="
      *     DISPLAY "RRKNUM     = " PTNYUINRRK-RRKNUM
      *     DISPLAY "RRKEDANUM  = " PTNYUINRRK-RRKEDANUM
      *     DISPLAY "NYUINYMD   = " PTNYUINRRK-NYUINYMD
      *     DISPLAY "TAIINYMD   = " PTNYUINRRK-TAIINYMD
      *     DISPLAY "TENNYUYMD  = " PTNYUINRRK-TENNYUYMD
      *     DISPLAY "TENSTUYMD  = " PTNYUINRRK-TENSTUYMD
      *
      *    平成２４年３月３１日時点で亜急性期入院医療管理料１を算定
      *    している状態か判定するサブを呼び出し
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "18"  OR  "54"
                   INITIALIZE                  ORCSAKYUSEI-AREA
                   MOVE    PTNYUINRRK-HOSPNUM  TO  LNK-AKYUSEI-HOSPNUM
                   MOVE    PTNYUINRRK-PTID     TO  LNK-AKYUSEI-PTID
                   MOVE    PTNYUINRRK-RRKNUM   TO  LNK-AKYUSEI-RRKNUM
                   CALL    "ORCSAKYUSEI"    USING  ORCSAKYUSEI-AREA
                                                   SPA-AREA
                   DISPLAY  "LNK-AKYUSEI-RC = " LNK-AKYUSEI-RC
               END-IF
           END-IF
      *
      *
           IF      SPA-GMN-I01-TOKNYUIN-CD    NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
      *        特定一般病棟入院料の場合は期間加算が存在するため、
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        療養病床で有床診療所入院基本料を相互算定する場合も
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        特定入院料の算定要件外に算定にする以下入院料を追加（平成２６年改正で対応）
      *        ・一般病棟特別入院基本料
      *        ・精神病棟１５対１入院基本料
      *        ・精神病棟特別入院基本料
      *        以下入院料を追加（平成２８年改正で対応）
      *        ・療養病棟・一般病棟７対１入院基本料
      *        ・療養病棟・一般病棟１０対１入院基本料
      *        ・療養病棟・一般病棟１３対１入院基本料
      *        ・療養病棟・一般病棟１５対１入院基本料
      *        以下入院料を追加（平成３０年改正で対応）
      *        ・療養病棟・急性期一般入院料２
      *        ・療養病棟・急性期一般入院料３
      *        ・療養病棟・急性期一般入院料４
      *        ・療養病棟・急性期一般入院料５
      *        ・療養病棟・急性期一般入院料６
      *        ・療養病棟・地域一般入院料１
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"   OR
                                                "099999955"   OR
                                                "099999956"   OR
                                                "099999957"   OR
      *                                         特定入院料の算定要件外の算定
                                                "190079010"   OR
                                                "190083810"   OR
                                                "190085010"   OR
      *                                         平成２８年改正対応
                                                "190198110"   OR
                                                "190198210"   OR
                                                "190198310"   OR
                                                "190198410"   OR
      *                                         平成３０年改正対応
                                                "190201910"   OR
                                                "190202010"   OR
                                                "190202110"   OR
                                                "190202210"   OR
                                                "190202310"   OR
                                                "190202410"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4017"      TO  SPA-ERRCD
                       GO  TO  4801-TENROOM-EDIT-EXT
                   END-IF
               END-IF
           ELSE
               PERFORM   4801-BTUSEL-SEC
               IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM   4801-RJNCD-CHG-SEC
                   PERFORM   4801-YAKINGEN-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4021"      TO  SPA-ERRCD
                       GO  TO  4801-TENROOM-EDIT-EXT
                   END-IF
               ELSE
                   IF    SYS-5001-BTU-SBT    =   "04" OR "09"
                       GO  TO  4801-TENROOM-EDIT-EXT
                   ELSE
                       MOVE    "4022"      TO  SPA-ERRCD
                       GO  TO  4801-TENROOM-EDIT-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    入院会計作成前に会計データの食事区分を退避
      *
           MOVE      SPACE                 TO  WRK-TENROOM-SKJ-TABLE
           INITIALIZE                          WRK-TENROOM-SKJ-TABLE
      *
           IF  SPA-NAI-I01-IDOYMD(1:6)  >= "200604"
            IF  PTNYUINRRK-SKJKBN       =  "A" OR  "B"
      *      前月末日の食事会計取得
      *     （異動日の入院会計が存在しない場合、前月末の食事情報取得の為）
             MOVE    SPA-NAI-I01-IDOYMD(1:6)
                                           TO  WRK-CHK-SRYYM
             IF      WRK-CHK-SRYMR         =   1
                 COMPUTE   WRK-CHK-SRYYR   =   WRK-CHK-SRYYR - 1
                 MOVE      12              TO  WRK-CHK-SRYMR
             ELSE
                 COMPUTE   WRK-CHK-SRYMR   =   WRK-CHK-SRYMR - 1
             END-IF
      *      前月の月末日を取得
             INITIALIZE                        STS-AREA-DAY
             INITIALIZE                        LNK-DAY7-AREA
             MOVE      "71"                TO  LNK-DAY7-IRAI
             MOVE    WRK-CHK-SRYYM         TO  LNK-DAY7-YM
             CALL    "ORCSDAY"       USING     STS-AREA-DAY
                                               LNK-DAY7-AREA
             MOVE    LNK-DAY7-KOYOMI       TO  IDX-MATSUBI
      *      異動日前月末日の食事会計（朝食）を読み込み
             INITIALIZE                        NYUINACCT-REC
             MOVE    PTNYUINRRK-HOSPNUM    TO  NACCT-HOSPNUM
             MOVE    PTNYUINRRK-PTID       TO  NACCT-PTID
             MOVE    WRK-CHK-SRYYM         TO  NACCT-SRYYM
             MOVE    "key30"               TO  MCP-PATHNAME
             PERFORM  910-NYUINACCT-SELECT-SEC
             MOVE    "key30"               TO  MCP-PATHNAME
             PERFORM  910-NYUINACCT-READ-SEC
             IF      FLG-NYUINACCT         =   ZERO
                 MOVE     MCPDATA-REC      TO  NYUINACCT-REC
                 MOVE     NACCT-DAY(IDX-MATSUBI)
                                           TO WRK-TENROOM-MAE-SKJ1-ASA
             END-IF
      *      入院会計マスタクローズ
             MOVE    "tbl_nyuinacct"       TO  MCP-TABLE
             MOVE    "key30"               TO  MCP-PATHNAME
             PERFORM 910-DBCLOSECURSOR-SEC
      *      異動日前月末日の食事会計（昼食）を読み込み
             INITIALIZE                        NYUINACCT-REC
             MOVE    PTNYUINRRK-HOSPNUM    TO  NACCT-HOSPNUM
             MOVE    PTNYUINRRK-PTID       TO  NACCT-PTID
             MOVE    WRK-CHK-SRYYM         TO  NACCT-SRYYM
             MOVE    "key31"               TO  MCP-PATHNAME
             PERFORM  910-NYUINACCT-SELECT-SEC
             MOVE    "key31"               TO  MCP-PATHNAME
             PERFORM  910-NYUINACCT-READ-SEC
             IF      FLG-NYUINACCT         =   ZERO
                 MOVE     MCPDATA-REC      TO  NYUINACCT-REC
                 MOVE     NACCT-DAY(IDX-MATSUBI)
                                           TO WRK-TENROOM-MAE-SKJ1-HIRU
             END-IF
      *      入院会計マスタクローズ
             MOVE    "tbl_nyuinacct"       TO  MCP-TABLE
             MOVE    "key31"               TO  MCP-PATHNAME
             PERFORM 910-DBCLOSECURSOR-SEC
      *      異動日前月末日の食事会計（夕食）を読み込み
             INITIALIZE                        NYUINACCT-REC
             MOVE    PTNYUINRRK-HOSPNUM    TO  NACCT-HOSPNUM
             MOVE    PTNYUINRRK-PTID       TO  NACCT-PTID
             MOVE    WRK-CHK-SRYYM         TO  NACCT-SRYYM
             MOVE    "key32"               TO  MCP-PATHNAME
             PERFORM  910-NYUINACCT-SELECT-SEC
             MOVE    "key32"               TO  MCP-PATHNAME
             PERFORM  910-NYUINACCT-READ-SEC
             IF      FLG-NYUINACCT         =   ZERO
                 MOVE     MCPDATA-REC      TO  NYUINACCT-REC
                 MOVE     NACCT-DAY(IDX-MATSUBI)
                                           TO WRK-TENROOM-MAE-SKJ1
                                              WRK-TENROOM-MAE-SKJ1-YORU
             END-IF
      *      入院会計マスタクローズ
             MOVE    "tbl_nyuinacct"       TO  MCP-TABLE
             MOVE    "key32"               TO  MCP-PATHNAME
             PERFORM 910-DBCLOSECURSOR-SEC
      *
      *      （異動月分）
             PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
               UNTIL   IDX-SKJ-TAIHI  >    3
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    SPA-NAI-I01-IDOYMD(1:6)
                                           TO  NACCT-SRYYM
                                               WRK-TENROOM-SKJ-SRYYMX
      *
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-SELECT-SEC
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT        =   ZERO
      *
      *          月末日を算定
                 INITIALIZE                  STS-AREA-DAY
                 INITIALIZE                  LNK-DAY7-AREA
                 MOVE      "71"          TO  LNK-DAY7-IRAI
                 MOVE    SPA-NAI-I01-IDOYMD(1:6)
                                         TO  LNK-DAY7-YM
                 CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                 LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI TO  IDX-MATSUBI
      *
                 MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *          食事を退避
                 MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    SPA-NAI-I01-IDOYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ18(1 IDX-SKJ)
                               IF    IDX-SKJ   =    IDX-MATSUBI
                                   MOVE    NACCT-DAY(IDX-SKJ)
                                         TO    WRK-TENROOM-MAE-SKJ2-ASA
                               END-IF
                        END-PERFORM
                     WHEN  2
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ19(1 IDX-SKJ)
                               IF    IDX-SKJ   =    IDX-MATSUBI
                                   MOVE    NACCT-DAY(IDX-SKJ)
                                         TO    WRK-TENROOM-MAE-SKJ2-HIRU
                               END-IF
                        END-PERFORM
                     WHEN  3
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ20(1 IDX-SKJ)
                               IF    IDX-SKJ   =    IDX-MATSUBI
                                   MOVE    NACCT-DAY(IDX-SKJ)
                                           TO  WRK-TENROOM-MAE-SKJ2
                                               WRK-TENROOM-MAE-SKJ2-YORU
                               END-IF
                        END-PERFORM
                 END-EVALUATE
               ELSE
                 MOVE    SPA-NAI-I01-IDOYMD(1:6)
                                         TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    SPA-NAI-I01-IDOYMD(7:2)
                                         TO  IDX-SHOKUJIX
      *          月末日を算定
                 INITIALIZE                  STS-AREA-DAY
                 INITIALIZE                  LNK-DAY7-AREA
                 MOVE      "71"          TO  LNK-DAY7-IRAI
                 MOVE    SPA-NAI-I01-IDOYMD(1:6)
                                         TO  LNK-DAY7-YM
                 CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                 LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI TO  IDX-MATSUBI
      *
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   IDX-MATSUBI
                              MOVE   WRK-TENROOM-MAE-SKJ1
                                        TO WRK-TENROOM-SKJ18(1 IDX-SKJ)
                              IF  PTNYUINRRK-SKJKBN  =  "B"
                                  MOVE   WRK-TENROOM-MAE-SKJ1-ASA
                                        TO WRK-TENROOM-SKJ18(1 IDX-SKJ)
                              END-IF
                        END-PERFORM
                     WHEN  2
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   IDX-MATSUBI
                              MOVE   WRK-TENROOM-MAE-SKJ1
                                        TO WRK-TENROOM-SKJ19(1 IDX-SKJ)
                              IF  PTNYUINRRK-SKJKBN  =  "B"
                                  MOVE   WRK-TENROOM-MAE-SKJ1-HIRU
                                        TO WRK-TENROOM-SKJ19(1 IDX-SKJ)
                              END-IF
                        END-PERFORM
                     WHEN  3
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   IDX-MATSUBI
                              MOVE   WRK-TENROOM-MAE-SKJ1
                                        TO WRK-TENROOM-SKJ20(1 IDX-SKJ)
                              IF  PTNYUINRRK-SKJKBN  =  "B"
                                  MOVE   WRK-TENROOM-MAE-SKJ1-YORU
                                        TO WRK-TENROOM-SKJ20(1 IDX-SKJ)
                              END-IF
                        END-PERFORM
                 END-EVALUATE
      *          異動日前月末日の食事（夕食）を異動翌月の食事退避項目へ
      *          セットしておく
                 MOVE    WRK-TENROOM-MAE-SKJ1  TO  WRK-TENROOM-MAE-SKJ2
                 MOVE    WRK-TENROOM-MAE-SKJ1-ASA
                                          TO  WRK-TENROOM-MAE-SKJ2-ASA
                 MOVE    WRK-TENROOM-MAE-SKJ1-HIRU
                                          TO  WRK-TENROOM-MAE-SKJ2-HIRU
                 MOVE    WRK-TENROOM-MAE-SKJ1-YORU
                                          TO  WRK-TENROOM-MAE-SKJ2-YORU
      *
                 MOVE    SPACE           TO  WRK-TENROOM-SKJ-SRYYM(1)
               END-IF
      *        入院会計マスタクローズ
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM 910-DBCLOSECURSOR-SEC
             END-PERFORM
      *      （異動翌月分）
             PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
                UNTIL   IDX-SKJ-TAIHI  >    3
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               IF   IDX-SKJ-TAIHI     =    1
                   IF      WRK-TENROOM-SKJ-SRYMM9  =  12
                       ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
                       MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
                   ELSE
                       ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
                   END-IF
               END-IF
               MOVE    WRK-TENROOM-SKJ-SRYYMX
                                           TO  NACCT-SRYYM
      *
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-SELECT-SEC
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT        =   ZERO
                 MOVE     MCPDATA-REC      TO  NYUINACCT-REC
      *          食事を退避
                 MOVE    NACCT-SRYYM       TO  WRK-TENROOM-SKJ-SRYYM(2)
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ18(2 IDX-SKJ)
                       END-PERFORM
                     WHEN  2
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ19(2 IDX-SKJ)
                       END-PERFORM
                     WHEN  3
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ20(2 IDX-SKJ)
                       END-PERFORM
                 END-EVALUATE
               ELSE
                 MOVE    WRK-TENROOM-SKJ-SRYYMX
                                        TO  WRK-TENROOM-SKJ-SRYYM(2)
      *          月末日を算定
                 INITIALIZE                  STS-AREA-DAY
                 INITIALIZE                  LNK-DAY7-AREA
                 MOVE      "71"          TO  LNK-DAY7-IRAI
                 MOVE    WRK-TENROOM-SKJ-SRYYMX
                                         TO  LNK-DAY7-YM
                 CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                 LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI TO  IDX-MATSUBI
      *
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                        PERFORM VARYING IDX-SKJ FROM 1 BY  1
                           UNTIL     IDX-SKJ   >   IDX-MATSUBI
                              MOVE   WRK-TENROOM-MAE-SKJ2
                                        TO WRK-TENROOM-SKJ18(2 IDX-SKJ)
                              IF   PTNYUINRRK-SKJKBN   =   "B"
                                  MOVE   WRK-TENROOM-MAE-SKJ2-ASA
                                        TO WRK-TENROOM-SKJ18(2 IDX-SKJ)
                              END-IF
                        END-PERFORM
                     WHEN  2
                        PERFORM VARYING IDX-SKJ FROM 1  BY  1
                           UNTIL     IDX-SKJ   >   IDX-MATSUBI
                              MOVE   WRK-TENROOM-MAE-SKJ2
                                        TO WRK-TENROOM-SKJ19(2 IDX-SKJ)
                              IF   PTNYUINRRK-SKJKBN   =   "B"
                                  MOVE   WRK-TENROOM-MAE-SKJ2-HIRU
                                        TO WRK-TENROOM-SKJ19(2 IDX-SKJ)
                              END-IF
                        END-PERFORM
                     WHEN  3
                        PERFORM VARYING IDX-SKJ FROM 1  BY  1
                           UNTIL     IDX-SKJ   >   IDX-MATSUBI
                              MOVE   WRK-TENROOM-MAE-SKJ2
                                        TO WRK-TENROOM-SKJ20(2 IDX-SKJ)
                              IF   PTNYUINRRK-SKJKBN   =   "B"
                                  MOVE   WRK-TENROOM-MAE-SKJ2-YORU
                                        TO WRK-TENROOM-SKJ20(2 IDX-SKJ)
                              END-IF
                        END-PERFORM
                 END-EVALUATE
               END-IF
      *        入院会計マスタクローズ
               MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM 910-DBCLOSECURSOR-SEC
             END-PERFORM
            END-IF
           ELSE
      *       ここから２００６年３月以前の食事退避処理
      *      （異動月分）
             INITIALIZE                      NYUINACCT-REC
             MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
             MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
             MOVE    SPA-NAI-I01-IDOYMD(1:6)
                                         TO  NACCT-SRYYM
                                             WRK-TENROOM-SKJ-SRYYMX
      *
             MOVE    NYUINACCT-REC       TO  MCPDATA-REC
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key28"             TO  MCP-PATHNAME
             PERFORM   910-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *          食事を退避
                 MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    SPA-NAI-I01-IDOYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                           UNTIL     IDX-SKJ   >   31
                      MOVE   NACCT-DAY(IDX-SKJ)
                                         TO  WRK-TENROOM-SKJ(1 IDX-SKJ)
                 END-PERFORM
             ELSE
                 MOVE    SPA-NAI-I01-IDOYMD(1:6)
                                         TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    SPA-NAI-I01-IDOYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                           UNTIL     IDX-SKJ   >   31
                      MOVE   ZERO        TO  WRK-TENROOM-SKJ(1 IDX-SKJ)
                 END-PERFORM
             END-IF
      *      入院会計マスタクローズ
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key28"             TO  MCP-PATHNAME
             PERFORM 910-DBCLOSECURSOR-SEC
      *      （異動翌月分）
             INITIALIZE                      NYUINACCT-REC
             MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
             MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
             IF      WRK-TENROOM-SKJ-SRYMM9  =  12
                 ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
                 MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
             ELSE
                 ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
             END-IF
             MOVE    WRK-TENROOM-SKJ-SRYYMX
                                         TO  NACCT-SRYYM
      *
             MOVE    NYUINACCT-REC       TO  MCPDATA-REC
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key28"             TO  MCP-PATHNAME
             PERFORM   910-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *          食事を退避
                 MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(2)
                 PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                      MOVE   NACCT-DAY(IDX-SKJ)
                                         TO  WRK-TENROOM-SKJ(2 IDX-SKJ)
                 END-PERFORM
             ELSE
                 MOVE    SPACE           TO  WRK-TENROOM-SKJ-SRYYM(2)
             END-IF
      *      入院会計マスタクローズ
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key28"             TO  MCP-PATHNAME
             PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *    入院会計作成前に会計データの外泊区分を退避
      *    （外泊異動月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SPA-NAI-I01-IDOYMD(1:6)
                                       TO  NACCT-SRYYM
                                           WRK-TENROOM-SKJ-SRYYMX
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        外泊を退避
               MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
               MOVE    SPA-NAI-I01-IDOYMD(7:2)
                                       TO  IDX-SHOKUJIX
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   NACCT-DAY(IDX-SKJ)
                                       TO WRK-TENROOM-GAIHAKU(1 IDX-SKJ)
               END-PERFORM
           ELSE
               MOVE    SPA-NAI-I01-IDOYMD(1:6)
                                       TO  WRK-TENROOM-SKJ-SRYYM(1)
               MOVE    SPA-NAI-I01-IDOYMD(7:2)
                                       TO  IDX-SHOKUJIX
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   ZERO        TO WRK-TENROOM-GAIHAKU(1 IDX-SKJ)
               END-PERFORM
           END-IF
      *    入院会計マスタクローズ
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *    （外泊異動翌月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           IF      WRK-TENROOM-SKJ-SRYMM9  =  12
               ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
               MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
           ELSE
               ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
           END-IF
           MOVE    WRK-TENROOM-SKJ-SRYYMX
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        外泊を退避
               PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   NACCT-DAY(IDX-SKJ)
                                       TO WRK-TENROOM-GAIHAKU(2 IDX-SKJ)
               END-PERFORM
           END-IF
      *    入院会計マスタクローズ
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *
      *    入院会計作成サブの呼出し
           INITIALIZE                     ORCSNYUACCT-AREA
           IF      FLG-SAI-SAKUSEI2   =   ZERO
               MOVE    "8"            TO  LNK-SCNYUACCT-SYORI-KBN
           ELSE
               MOVE    "9"            TO  LNK-SCNYUACCT-SYORI-KBN
               MOVE    ZERO           TO  FLG-SAI-SAKUSEI2
           END-IF
           MOVE    "1"                TO  SPA-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE    PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                           TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                           TO   LNK-SCNYUACCT-STSRYYMD
           MOVE    SPA-NAI-I01-IDOYMD
                           TO   LNK-SCNYUACCT-EDSRYYMD
           PERFORM   VARYING   IDX6   FROM   1   BY   1
               UNTIL (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE) OR
                     (IDX6   >     7)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX6)
           END-PERFORM
      *
      *    医療観察入院セット
           INITIALIZE            ORCSIRYOKNS-AREA
           MOVE     PTNYUINRRK-HOSPNUM   TO  LNK-IRYOKNS-HOSPNUM
           MOVE     PTNYUINRRK-TOKUTEI-KBN
                                         TO  LNK-IRYOKNS-KBN
           MOVE     PTNYUINRRK-TOKUTEI-NYUIN
                                         TO  LNK-IRYOKNS-NYUIN
           MOVE     PTNYUINRRK-BTUNUM    TO  LNK-IRYOKNS-BTUNUM
      *     DISPLAY  "==  ORCSIRYOKNS TENROOM-CALL ==="
      *     DISPLAY  "SYOKIKSN-IN-SANTEISTYMD = " SYOKIKSN-IN-SANTEISTYMD
      *     DISPLAY  "SPA-NAI-I01-NYUINYMD    = " SPA-NAI-I01-NYUINYMD
      *     DISPLAY  "SPA-NAI-I01-IDOYMD      = " SPA-NAI-I01-IDOYMD
           MOVE     SYOKIKSN-IN-SANTEISTYMD(1:6)
                                         TO  LNK-IRYOKNS-SANTEIYM
           CALL     "ORCSIRYOKNS"   USING   ORCSIRYOKNS-AREA
                                            SPA-AREA
      *     DISPLAY   "LNK-IRYOKNS-RC = " LNK-IRYOKNS-RC
           IF       LNK-IRYOKNS-RC     =    ZERO
               MOVE   LNK-IRYOKNS-SRYCD(1)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   LNK-IRYOKNS-SRYCD(2)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(2)
               MOVE   LNK-IRYOKNS-SRYCD(3)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(3)
               MOVE   LNK-IRYOKNS-SRYCD(4)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(4)
               MOVE   LNK-IRYOKNS-SRYCD(5)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(5)
               MOVE   SPA-NAI-I01-IDOYMD
                                  TO  LNK-SCNYUACCT-STSRYYMD
                                      LNK-SCNYUACCT-EDSRYYMD
           END-IF
      *
      *    通算算定済日数
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    特定入院で入院料が変更になるコードの対応
      *
      *    特定入院で入院料が変更になるコードの対応
      *
      *     DISPLAY "===  TEST TENROOM-EDIT ==="
           INITIALIZE                       ORCSTOKNYUIN2-AREA
           MOVE   PTNYUINRRK-HOSPNUM    TO  LNK-TOKNYUIN2-HOSPNUM
           MOVE   PTNYUINRRK-PTID       TO  LNK-TOKNYUIN2-PTID
           MOVE   "1"                   TO  LNK-TOKNYUIN2-IDOKBN
           MOVE   PTNYUINRRK-NYUINYMD   TO  LNK-TOKNYUIN2-NYUINYMD
           MOVE   SPA-NAI-I01-IDOYMD    TO  LNK-TOKNYUIN2-SANTEIYMD
           MOVE   SYOKIKSN-IN-KHNSRYCD  TO  LNK-TOKNYUIN2-SET-SRYCD
           MOVE   STUSAN-NISSU08        TO  LNK-TOKNYUIN2-NISSU08
           MOVE   PTNYUINRRK-SHONUM     TO  LNK-TOKNYUIN2-SHONUM
           MOVE   PTNYUINRRK-TOKUTEI-NYUIN
                             TO  LNK-TOKNYUIN2-TOKUTEI-NYUIN
           IF    (LNK-AKYUSEI-RC        =   1)           AND
                 (SPA-NAI-I01-IDOYMD    >=   "20120401")
              MOVE   "1"      TO   LNK-TOKNYUIN2-TOKUBETSU-FLG(1)
           END-IF
      *    療養病棟（病床）入院のフラグセット
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-5001-KBNCD
           MOVE    SPA-NAI-I01-IDOYMD  TO  SYS-5001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD (7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
               IF    SYS-5001-BTU-SBT    =   "04"  OR  "09"
                       MOVE    "1"     TO
                                         LNK-TOKNYUIN2-TOKUBETSU-FLG(2)
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CALL  "ORCSTOKNYUIN2"     USING  ORCSTOKNYUIN2-AREA
                                            SPA-AREA
           IF     LNK-TOKNYUIN2-RC      =   ZERO
              MOVE   SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
              PERFORM   VARYING   IDX    FROM    1   BY  1
                UNTIL   IDX       >    5
                  IF   LNK-TOKNYUIN2-SRYCD(IDX)  NOT =   SPACE
                    MOVE  LNK-TOKNYUIN2-SRYCD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                    MOVE  LNK-TOKNYUIN2-STYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX)
                    MOVE  LNK-TOKNYUIN2-EDYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX)
                  END-IF
              END-PERFORM
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                          TO  IPN-NYUKAIKEI-SAKUSEI-FLG
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                          TO  IPN-SANTEISTYMD
           ELSE
              IF   PTNYUINRRK-TOKUTEI-NYUIN  NOT =   SPACE
                IF    LNK-TOKNYUIN2-RC  =   1
                    MOVE    SPACE         TO  SPA-GMN-I01-TOKNYUIN-CD
                                              SYOKIKSN-IN-KHNSRYCD
                                          LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                END-IF
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                        TO  IPN-NYUKAIKEI-SAKUSEI-FLG
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                        TO  IPN-SANTEISTYMD
              END-IF
           END-IF
      *
      *
      *-------特定入院対応ここまで-----------------------------------
      *
      *    入院料加算診療コード取り込み
           PERFORM    4801-NYUKSN-GET-SEC
      *
      *    特定入院料区分のセット
           IF      SPA-GMN-I01-TOKNYUIN-CD    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        特定入院基本料、療養病棟入院基本料、療養病床入院基本料等は
      *        特定入院料扱いから外す
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
      *            特定入院基本料（一般病棟入院基本料）はマスタが
      *            平成２６年９月３０日までの為、参照時の日付を変更
                   IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                       MOVE    "20140930"   TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   END-IF
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
               IF    LNK-SCNYUACCT-KAGEN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *    外泊
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9
                                TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    食事区分
           MOVE   "099999913"   TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
      ***     IF     WRK-TAIHI-SHOKUJI    NOT =   ZERO
      ***            MOVE   WRK-TAIHI-SHOKUJI
      ***                          TO  LNK-SCNYUACCT-SHOKUJIKBN
      ***     ELSE
      ***            MOVE   1      TO  LNK-SCNYUACCT-SHOKUJIKBN
      ***     END-IF
           EVALUATE   PTNYUINRRK-SKJKBN
               WHEN   "0"
                      MOVE   0     TO  LNK-SCNYUACCT-SHOKUJIKBN
               WHEN   "1"
                      MOVE   1     TO  LNK-SCNYUACCT-SHOKUJIKBN
               WHEN   "2"
                      MOVE   2     TO  LNK-SCNYUACCT-SHOKUJIKBN
               WHEN   "3"
                      MOVE   3     TO  LNK-SCNYUACCT-SHOKUJIKBN
               WHEN   "C"
                   MOVE     "1"    TO  LNK-SCNYUACCT-SHOKUJI-SETKBN
                   EVALUATE   PTNYUINRRK-SKJKBN-ASA
                      WHEN   "0"
                           MOVE   0   TO  LNK-SCNYUACCT-SHOKUJIKBN-ASA
                      WHEN   "1"
                           MOVE   1   TO  LNK-SCNYUACCT-SHOKUJIKBN-ASA
                      WHEN   "2"
                           MOVE   2   TO  LNK-SCNYUACCT-SHOKUJIKBN-ASA
                      WHEN   "3"
                           MOVE   3   TO  LNK-SCNYUACCT-SHOKUJIKBN-ASA
                   END-EVALUATE
                   EVALUATE   PTNYUINRRK-SKJKBN-HIRU
                      WHEN   "0"
                           MOVE   0   TO  LNK-SCNYUACCT-SHOKUJIKBN-HIRU
                      WHEN   "1"
                           MOVE   1   TO  LNK-SCNYUACCT-SHOKUJIKBN-HIRU
                      WHEN   "2"
                           MOVE   2   TO  LNK-SCNYUACCT-SHOKUJIKBN-HIRU
                      WHEN   "3"
                           MOVE   3   TO  LNK-SCNYUACCT-SHOKUJIKBN-HIRU
                   END-EVALUATE
                   EVALUATE   PTNYUINRRK-SKJKBN-YU
                      WHEN   "0"
                           MOVE   0   TO  LNK-SCNYUACCT-SHOKUJIKBN-YORU
                      WHEN   "1"
                           MOVE   1   TO  LNK-SCNYUACCT-SHOKUJIKBN-YORU
                      WHEN   "2"
                           MOVE   2   TO  LNK-SCNYUACCT-SHOKUJIKBN-YORU
                      WHEN   "3"
                           MOVE   3   TO  LNK-SCNYUACCT-SHOKUJIKBN-YORU
                   END-EVALUATE
           END-EVALUATE
      *    入院日と異動日が同日である場合、入院時にセットした
      *    「入院日食事開始時間区分」以降が食事の更新対象となる為
      *    該当日の朝食、昼食、夕食を更新後に戻せるように退避しておく
           MOVE    ZERO               TO  WRK-IDOBI-SKJ18
                                          WRK-IDOBI-SKJ19
                                          WRK-IDOBI-SKJ20
           IF   PTNYUINRRK-NYUINYMD   =   SPA-NAI-I01-IDOYMD
              IF   PTNYUINRRK-TIMEKBN-IN   =   "1"
      *            朝食からの算定時は処理をスルーする
                   CONTINUE
              ELSE
                PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
                  UNTIL   IDX-SKJ-TAIHI  >    3
                  INITIALIZE                      NYUINACCT-REC
                  MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                  MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                  MOVE    SPA-NAI-I01-IDOYMD(1:6)
                                              TO  NACCT-SRYYM
                  MOVE    SPA-NAI-I01-IDOYMD  TO  WRK-IDOBI-SKJ-SRYYMD
      *
                  EVALUATE   IDX-SKJ-TAIHI
                      WHEN    1
                          MOVE    "key30"     TO  MCP-PATHNAME
                      WHEN    2
                          MOVE    "key31"     TO  MCP-PATHNAME
                      WHEN    3
                          MOVE    "key32"     TO  MCP-PATHNAME
                  END-EVALUATE
                  PERFORM  910-NYUINACCT-SELECT-SEC
                  EVALUATE   IDX-SKJ-TAIHI
                      WHEN    1
                          MOVE    "key30"     TO  MCP-PATHNAME
                      WHEN    2
                          MOVE    "key31"     TO  MCP-PATHNAME
                      WHEN    3
                          MOVE    "key32"     TO  MCP-PATHNAME
                  END-EVALUATE
                  PERFORM  910-NYUINACCT-READ-SEC
                  IF      FLG-NYUINACCT        =   ZERO
                    MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *             異動日の食事を退避
                    EVALUATE  IDX-SKJ-TAIHI
                      WHEN  1
                           MOVE   NACCT-DAY(WRK-IDOBI-SKJ-SRYDD9)
                                         TO WRK-IDOBI-SKJ18
                      WHEN  2
                           MOVE   NACCT-DAY(WRK-IDOBI-SKJ-SRYDD9)
                                         TO WRK-IDOBI-SKJ19
                      WHEN  3
                           MOVE   NACCT-DAY(WRK-IDOBI-SKJ-SRYDD9)
                                         TO WRK-IDOBI-SKJ20
                    END-EVALUATE
                  END-IF
      *           入院会計マスタクローズ
                  MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                  EVALUATE   IDX-SKJ-TAIHI
                      WHEN    1
                          MOVE    "key30"     TO  MCP-PATHNAME
                      WHEN    2
                          MOVE    "key31"     TO  MCP-PATHNAME
                      WHEN    3
                          MOVE    "key32"     TO  MCP-PATHNAME
                  END-EVALUATE
                  PERFORM 910-DBCLOSECURSOR-SEC
                END-PERFORM
              END-IF
           END-IF
      *
      *    食堂加算
           PERFORM     4801-BTUSEL-SEC
           IF   FLG-SYSKANRI           =    ZERO
               IF   SYS-5001-BTU-SYOKUDOKBN       =    "2"
                    MOVE     "197000570"    TO
                                LNK-SCNYUACCT-SHOKUDO-SRYCD
                    MOVE    1   TO   LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    保険組合せ
           MOVE   "099999914"   TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE   PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
      *    転科元入院科
           MOVE   "00"          TO  LNK-SCNYUACCT-MOTO-NYUINKA
      **     MOVE   SPA-GMN-I01-NYUINKA-MOT (1 : 2)
      **                          TO  LNK-SCNYUACCT-MOTO-NYUINKA
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           IF     PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
      *    入院カレンダー未設定の判定
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え
           IF    (LNK-SCNYUACCT-STSRYYMD   >=   "20120401")  AND
                 (LNK-SCNYUACCT-STSRYYMD   <=   "20140331")
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え（平成２６年改定）
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20140401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169610"
                             MOVE "190170810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169710"
                             MOVE "190170910" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169810"
                             MOVE "190171010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169910"
                             MOVE "190171110" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170010"
                             MOVE "190171210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170110"
                             MOVE "190171310" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170210"
                             MOVE "190171410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170310"
                             MOVE "190171510" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170410"
                             MOVE "190171610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    特定入院基本料のコード細分化置き換え
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161010" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161210" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
      *
      *            特定機能病院は病棟設定された入院コードから判定
                   IF   SYS-5001-BTU-KHNSRYCD =  "190118610" OR
                                                 "190705610" OR
                                                 "190085910"
                     MOVE "190160810" TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-IF
               END-IF
      *        特別入院基本料
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799510"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161110" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161310" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE   PTNYUINRRK-BTUNUM   TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
               DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                         LNK-SCNYUACCT-RC
           END-IF
      *
      *    入院会計作成後に退避データの食事区分を復元
           IF  SPA-NAI-I01-IDOYMD(1:6)  >= "200604"
             IF  PTNYUINRRK-SKJKBN       =  "A"  OR  "B"
      *       （異動月分）
               IF   WRK-TENROOM-SKJ-SRYYM(1)  NOT =   SPACE
                PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                 INITIALIZE                      NYUINACCT-REC
                 MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                 MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                 MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                             TO  NACCT-SRYYM
      *
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM  910-NYUINACCT-SELECT-SEC
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM  910-NYUINACCT-READ-SEC
                 IF      FLG-NYUINACCT        =   ZERO
                     MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *              食事を復元
                     MOVE    SPA-NAI-I01-IDOYMD(7:2)
                                         TO  IDX-SHOKUJIX
                     EVALUATE  IDX-SKJ-TAIHI
                         WHEN  1
                          PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ18(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                         WHEN  2
                          PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ19(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                         WHEN  3
                          PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ20(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                     END-EVALUATE
      *              剤回数再セット
                     MOVE  ZERO              TO  NACCT-ZAIKAISU
                     PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                              UNTIL     IDX-SKJ   >   31
                         IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                             ADD   1        TO  NACCT-ZAIKAISU
                         END-IF
                     END-PERFORM   
                 END-IF
                 MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                 MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                 MOVE     "key"              TO  MCP-PATHNAME
                 PERFORM 910-DBUPDATE-SEC
      *          入院会計マスタクローズ
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM 910-DBCLOSECURSOR-SEC
                END-PERFORM
               END-IF
      *
      *       （異動翌月分）
               IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
               PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                   MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                               TO  NACCT-SRYYM
      *
      *
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM  910-NYUINACCT-SELECT-SEC
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM  910-NYUINACCT-READ-SEC
                   IF      FLG-NYUINACCT        =   ZERO
                       MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *                食事を復元
                       EVALUATE  IDX-SKJ-TAIHI
                           WHEN  1
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ18(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                           WHEN  2
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ19(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                           WHEN  3
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ20(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                       END-EVALUATE
      *                剤回数再セット
                       MOVE  ZERO              TO  NACCT-ZAIKAISU
                       PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                           IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1        TO  NACCT-ZAIKAISU
                           END-IF
                       END-PERFORM   
                   END-IF
                   MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                   MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                   MOVE     "key"              TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
      *            入院会計マスタクローズ
                   MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-PERFORM
               END-IF
             ELSE
      *        入院日と異動日が同日である場合、退避した食事を入院日食事
      *        時間より前の部分について退避したデータより復元する
               IF   PTNYUINRRK-NYUINYMD   =   SPA-NAI-I01-IDOYMD
                  IF   PTNYUINRRK-TIMEKBN-IN   =   "1"
      *             朝食からの算定時は処理をスルーする
                    CONTINUE
                  ELSE
      *             入院日食事時間区分が0:食なし−朝食、昼食、夕食を戻す
      *             入院日食事時間区分が2:昼から−朝食を戻す
      *             入院日食事時間区分が3:夕から−朝食、昼食を戻す
      *
      *             朝食の戻し処理
                    IF   PTNYUINRRK-TIMEKBN-IN   =   "0" OR "2" OR "3"
                      INITIALIZE                      NYUINACCT-REC
                      MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                      MOVE    PTNYUINRRK-PTID      TO  NACCT-PTID
                      MOVE    WRK-IDOBI-SKJ-SRYYMD(1:6)
                                                   TO  NACCT-SRYYM
                      MOVE    "key30"              TO  MCP-PATHNAME
                      PERFORM  910-NYUINACCT-SELECT-SEC
                      MOVE    "key30"     TO  MCP-PATHNAME
                      PERFORM  910-NYUINACCT-READ-SEC
      *               食事を復元
                      MOVE   WRK-IDOBI-SKJ18       TO  NACCT-DAY
                                                 (WRK-IDOBI-SKJ-SRYDD9)
      *               剤回数再セット
                      MOVE  ZERO                   TO  NACCT-ZAIKAISU
                      PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                         IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                             ADD   1        TO  NACCT-ZAIKAISU
                         END-IF
                      END-PERFORM
                      MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                      MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                      MOVE     "key"              TO  MCP-PATHNAME
                      PERFORM 910-DBUPDATE-SEC
      *               入院会計マスタクローズ
                      MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                      MOVE    "key30"             TO  MCP-PATHNAME
                      PERFORM 910-DBCLOSECURSOR-SEC
                    END-IF
      *
      *             昼食の戻し処理
                    IF   PTNYUINRRK-TIMEKBN-IN   =   "0" OR "3"
                      INITIALIZE                      NYUINACCT-REC
                      MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                      MOVE    PTNYUINRRK-PTID      TO  NACCT-PTID
                      MOVE    WRK-IDOBI-SKJ-SRYYMD(1:6)
                                                   TO  NACCT-SRYYM
                      MOVE    "key31"              TO  MCP-PATHNAME
                      PERFORM  910-NYUINACCT-SELECT-SEC
                      MOVE    "key31"     TO  MCP-PATHNAME
                      PERFORM  910-NYUINACCT-READ-SEC
      *               食事を復元
                      MOVE   WRK-IDOBI-SKJ19       TO  NACCT-DAY
                                                 (WRK-IDOBI-SKJ-SRYDD9)
      *               剤回数再セット
                      MOVE  ZERO                   TO  NACCT-ZAIKAISU
                      PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                         IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                             ADD   1        TO  NACCT-ZAIKAISU
                         END-IF
                      END-PERFORM
                      MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                      MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                      MOVE     "key"              TO  MCP-PATHNAME
                      PERFORM 910-DBUPDATE-SEC
      *               入院会計マスタクローズ
                      MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                      MOVE    "key31"             TO  MCP-PATHNAME
                      PERFORM 910-DBCLOSECURSOR-SEC
                    END-IF
      *
      *             夕食の戻し処理
                    IF   PTNYUINRRK-TIMEKBN-IN   =   "0"
                      INITIALIZE                      NYUINACCT-REC
                      MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                      MOVE    PTNYUINRRK-PTID      TO  NACCT-PTID
                      MOVE    WRK-IDOBI-SKJ-SRYYMD(1:6)
                                                   TO  NACCT-SRYYM
                      MOVE    "key32"              TO  MCP-PATHNAME
                      PERFORM  910-NYUINACCT-SELECT-SEC
                      MOVE    "key32"     TO  MCP-PATHNAME
                      PERFORM  910-NYUINACCT-READ-SEC
      *               食事を復元
                      MOVE   WRK-IDOBI-SKJ20       TO  NACCT-DAY
                                                 (WRK-IDOBI-SKJ-SRYDD9)
      *               剤回数再セット
                      MOVE  ZERO                   TO  NACCT-ZAIKAISU
                      PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                         IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                             ADD   1        TO  NACCT-ZAIKAISU
                         END-IF
                      END-PERFORM
                      MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                      MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                      MOVE     "key"              TO  MCP-PATHNAME
                      PERFORM 910-DBUPDATE-SEC
      *               入院会計マスタクローズ
                      MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                      MOVE    "key32"             TO  MCP-PATHNAME
                      PERFORM 910-DBCLOSECURSOR-SEC
                    END-IF
                  END-IF
               END-IF
             END-IF
           ELSE
      *       （異動月分）
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            食事を復元
                   MOVE    SPA-NAI-I01-IDOYMD(7:2)
                                       TO  IDX-SHOKUJIX
                   PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                             UNTIL     IDX-SKJ   >   31
                         MOVE   WRK-TENROOM-SKJ(1 IDX-SKJ)
                                           TO  NACCT-DAY(IDX-SKJ)
                   END-PERFORM
      *            剤回数再セット
                   MOVE  ZERO              TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM   
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
      *        入院会計マスタクローズ
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
      *       （異動翌月分）
               IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                   MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                               TO  NACCT-SRYYM
      *
                   MOVE    NYUINACCT-REC       TO  MCPDATA-REC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key28"             TO  MCP-PATHNAME
                   PERFORM   910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *                食事を復元
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   WRK-TENROOM-SKJ(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                       END-PERFORM
      *                剤回数再セット
                       MOVE  ZERO              TO  NACCT-ZAIKAISU
                       PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                           IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1        TO  NACCT-ZAIKAISU
                           END-IF
                       END-PERFORM   
                   END-IF
                   MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key28"             TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *
      *    入院会計作成後に退避データの外泊区分を復元
      *    （異動月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        食事を復元
               MOVE    SPA-NAI-I01-IDOYMD(7:2)
                                       TO  IDX-SHOKUJIX
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                     MOVE   WRK-TENROOM-GAIHAKU(1 IDX-SKJ)
                                       TO  NACCT-DAY(IDX-SKJ)
               END-PERFORM
      *        剤回数再セット
               MOVE  ZERO              TO  NACCT-ZAIKAISU
               PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                        UNTIL     IDX-SKJ   >   31
                   IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                       ADD   1        TO  NACCT-ZAIKAISU
                   END-IF
               END-PERFORM   
           END-IF
           MOVE     NYUINACCT-REC      TO  MCPDATA-REC
           MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
           MOVE     "key"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
      *    入院会計マスタクローズ
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *   （異動翌月分）
           IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM   910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            食事を復元
                   PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                             UNTIL     IDX-SKJ   >   31
                        MOVE   WRK-TENROOM-GAIHAKU(2 IDX-SKJ)
                                           TO  NACCT-DAY(IDX-SKJ)
                   END-PERFORM
      *            剤回数再セット
                   MOVE  ZERO              TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                             UNTIL     IDX-SKJ   >   31
                       IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM   
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *    入院会計作成後に食事の算定が無い日の食堂加算をクリアする
      *    処理を追加
           IF  SPA-NAI-I01-IDOYMD(1:6)     >= "200604"
      *        （異動月分）
               MOVE      SPACE             TO   WRK-SHOKUDOU-SRYYM
               IF        WRK-TENROOM-SKJ-SRYYM(1)   NOT =   SPACE
                   MOVE  WRK-TENROOM-SKJ-SRYYM(1)
                                           TO   WRK-SHOKUDOU-SRYYM
                   PERFORM   4801-SHOKUDOU-CLEAR-SEC
               END-IF
      *        （異動翌月分）
               MOVE      SPACE             TO   WRK-SHOKUDOU-SRYYM
               IF        WRK-TENROOM-SKJ-SRYYM(2)   NOT =   SPACE
                   MOVE  WRK-TENROOM-SKJ-SRYYM(2)
                                           TO   WRK-SHOKUDOU-SRYYM
                   PERFORM   4801-SHOKUDOU-CLEAR-SEC
               END-IF
      *        （異動時の食事区分が食なしの異動翌月分）
               MOVE      SPACE             TO   WRK-SHOKUDOU-SRYYM
               IF       (WRK-TENROOM-SKJ-SRYYM(1) NOT =   SPACE)  AND
                        (WRK-TENROOM-SKJ-SRYYM(2)     =   SPACE)  AND
                        (PTNYUINRRK-SKJKBN            =   "0")
                   MOVE  WRK-TENROOM-SKJ-SRYYM(1)
                                           TO   WRK-SHOKUDOU-SRYYM
                   IF    WRK-SHOKUDOU-SRYMM   =   12
                         ADD     1         TO  WRK-SHOKUDOU-SRYYY
                         MOVE    1         TO  WRK-SHOKUDOU-SRYMM
                   ELSE
                         ADD     1         TO  WRK-SHOKUDOU-SRYMM
                   END-IF
                   PERFORM   4801-SHOKUDOU-CLEAR-SEC
               END-IF
           END-IF
      *
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
               MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
               MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
               MOVE    IPN-SANTEISTYMD   TO   SPA-NAI-I01-IDOYMD
               MOVE    IPN-SANTEISTYMD   TO   PTNYUINRRK-TENNYUYMD
           END-IF
      *
           .
       4801-TENROOM-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    食堂加算クリア処理
      *****************************************************************
       4801-SHOKUDOU-CLEAR-SEC       SECTION.
      *
      *    該当月の食堂加算算定有無チェック
           MOVE    ZERO                TO  FLG-SHOKUDOU
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-SHOKUDOU-SRYYM  TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key43"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE     1              TO  FLG-SHOKUDOU
           END-IF
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key43"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    食事（朝、昼、夕）から食事算定日を特定
           IF      FLG-SHOKUDOU         =  1
               MOVE    SPACE               TO  WRK-SHOKUDOU-SKJTBL
      *        （朝食）
               INITIALIZE                  NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-SHOKUDOU-SRYYM  TO  NACCT-SRYYM
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-SELECT-SEC
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT       =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                   PERFORM   VARYING  IDX-SHOKUDOU   FROM  1  BY  1
                     UNTIL   IDX-SHOKUDOU   >   31
                     IF   NACCT-DAY(IDX-SHOKUDOU)   NOT =  ZERO
                         MOVE  "1"         TO
                                     WRK-SHOKUDOU-SKJ-ARI(IDX-SHOKUDOU)
                     END-IF
                   END-PERFORM
               END-IF
      *        入院会計マスタクローズ
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *        （昼食）
               INITIALIZE                  NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-SHOKUDOU-SRYYM  TO  NACCT-SRYYM
               MOVE    "key31"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-SELECT-SEC
               MOVE    "key31"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT       =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                   PERFORM   VARYING  IDX-SHOKUDOU   FROM  1  BY  1
                     UNTIL   IDX-SHOKUDOU   >   31
                     IF   NACCT-DAY(IDX-SHOKUDOU)   NOT =  ZERO
                         MOVE  "1"         TO
                                     WRK-SHOKUDOU-SKJ-ARI(IDX-SHOKUDOU)
                     END-IF
                   END-PERFORM
               END-IF
      *        入院会計マスタクローズ
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key31"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *        （夕食）
               INITIALIZE                  NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-SHOKUDOU-SRYYM  TO  NACCT-SRYYM
               MOVE    "key32"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-SELECT-SEC
               MOVE    "key32"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT       =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                   PERFORM   VARYING  IDX-SHOKUDOU   FROM  1  BY  1
                     UNTIL   IDX-SHOKUDOU   >   31
                     IF   NACCT-DAY(IDX-SHOKUDOU)   NOT =  ZERO
                         MOVE  "1"         TO
                                     WRK-SHOKUDOU-SKJ-ARI(IDX-SHOKUDOU)
                     END-IF
                   END-PERFORM
               END-IF
      *        入院会計マスタクローズ
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key32"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
      *        食堂加算のカレンダー更新
               MOVE    ZERO                TO  FLG-SHOKUDOU
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-SHOKUDOU-SRYYM  TO  NACCT-SRYYM
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key43"             TO  MCP-PATHNAME
               PERFORM   910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
                   PERFORM   VARYING  IDX-SHOKUDOU   FROM  1  BY  1
                     UNTIL   IDX-SHOKUDOU   >   31
                        IF  WRK-SHOKUDOU-SKJ-ARI(IDX-SHOKUDOU)  =  SPACE
                           MOVE   ZERO    TO  NACCT-DAY(IDX-SHOKUDOU)
                        END-IF
                   END-PERFORM
      *            剤回数再セット
                   MOVE  ZERO              TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SHOKUDOU FROM  1  BY  1
                     UNTIL  IDX-SHOKUDOU   >   31
                       IF   NACCT-DAY(IDX-SHOKUDOU)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key43"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
           .
       4801-SHOKUDOU-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    異動取消時会計テーブル編集処理
      *****************************************************************
       4801-IDOKAIKEI-CANCEL-SEC       SECTION.
      *
           MOVE   SPACE                TO  WRK-DOU-TENNYUYMD
      *    入院会計作成サブの呼出し（異動分の会計削除）
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     "4"     TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  LNK-SCNYUACCT-EDSRYYMD
      *
      *    入院会計削除前に会計データの食事区分と保険組合せを退避
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    "00"                TO  NACCT-SRYKA
           MOVE    LNK-SCNYUACCT-EDSRYYMD(1:6)
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO        TO      WRK-TAIHI-SHOKUJI
               PERFORM UNTIL       MCP-RC       NOT =   ZERO
      *
                   MOVE     MCPDATA-REC   TO   NYUINACCT-REC
      *
      *            食事を退避
                   IF      NACCT-ZAISKBKBN     =   "4"
                       MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                         TO    IDX-SHOKUJIX
                       IF      NACCT-DAY(IDX-SHOKUJI9)  NOT =   ZERO
                            MOVE   NACCT-DAY(IDX-SHOKUJI9) TO
                                                  WRK-TAIHI-SHOKUJI
                       END-IF
                   END-IF
      *            保険を退避
                   IF      NACCT-ZAISKBKBN     =   "5"
                       MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                         TO    IDX-SHOKUJIX
                       IF      NACCT-DAY(IDX-SHOKUJI9)  NOT =   ZERO
                            MOVE   NACCT-DAY(IDX-SHOKUJI9) TO
                                                  WRK-TAIHI-HKN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key16"             TO  MCP-PATHNAME
                   PERFORM 910-DBFETCH-SEC
               END-PERFORM
           ELSE
               INITIALIZE                  NYUINACCT-REC
           END-IF
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    入院会計作成前に会計データの食事区分を退避
           IF  LNK-SCNYUACCT-EDSRYYMD(1:6)  >= "200604"
      *      （異動月分）
             PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
               UNTIL   IDX-SKJ-TAIHI  >    3
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    SPA-NAI-I01-MAEIDOYMD(1:6)
                                           TO  NACCT-SRYYM
                                               WRK-TENROOM-SKJ-SRYYMX
      *
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-SELECT-SEC
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT        =   ZERO
                 MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *          食事を退避
                 MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ18(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  2
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ19(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  3
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ20(1 IDX-SKJ)
                        END-PERFORM
                 END-EVALUATE
               ELSE
                 MOVE    LNK-SCNYUACCT-EDSRYYMD(1:6)
                                         TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ18(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  2
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ19(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  3
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ20(1 IDX-SKJ)
                        END-PERFORM
                 END-EVALUATE
                 MOVE    SPACE           TO  WRK-TENROOM-SKJ-SRYYM(1)
               END-IF
      *        入院会計マスタクローズ
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM 910-DBCLOSECURSOR-SEC
             END-PERFORM
      *     （異動翌月分）
             PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
                UNTIL   IDX-SKJ-TAIHI  >    3
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               IF   IDX-SKJ-TAIHI     =    1
                   IF      WRK-TENROOM-SKJ-SRYMM9  =  12
                       ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
                       MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
                   ELSE
                       ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
                   END-IF
               END-IF
               MOVE    WRK-TENROOM-SKJ-SRYYMX
                                           TO  NACCT-SRYYM
      *
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-SELECT-SEC
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT        =   ZERO
                 MOVE     MCPDATA-REC      TO  NYUINACCT-REC
      *          食事を退避
                 MOVE    NACCT-SRYYM       TO  WRK-TENROOM-SKJ-SRYYM(2)
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ18(2 IDX-SKJ)
                       END-PERFORM
                     WHEN  2
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ19(2 IDX-SKJ)
                       END-PERFORM
                     WHEN  3
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ20(2 IDX-SKJ)
                       END-PERFORM
                 END-EVALUATE
               ELSE
                   MOVE    SPACE           TO  WRK-TENROOM-SKJ-SRYYM(2)
               END-IF
      *        入院会計マスタクローズ
               MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM 910-DBCLOSECURSOR-SEC
             END-PERFORM
           ELSE
      *        （異動月分）
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    SPA-NAI-I01-MAEIDOYMD(1:6)
                                           TO  NACCT-SRYYM
                                               WRK-TENROOM-SKJ-SRYYMX
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            食事を退避
                   MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
                   MOVE    SPA-NAI-I01-MAEIDOYMD(7:2)
                                           TO  IDX-SHOKUJIX
                   MOVE    SPA-NAI-I01-MAEIDOYMD(7:2)
                                           TO  WRK-TENROOM-SKJ-DD(1)
                   PERFORM  VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                            UNTIL     IDX-SKJ   >   31
                        MOVE   NACCT-DAY(IDX-SKJ)
                                       TO  WRK-TENROOM-SKJ(1 IDX-SKJ)
                   END-PERFORM
               END-IF
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *        （異動翌月分）
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               IF      WRK-TENROOM-SKJ-SRYMM9  =  12
                   ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
                   MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
               ELSE
                   ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
               END-IF
               MOVE    WRK-TENROOM-SKJ-SRYYMX
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            食事を退避
                   MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(2)
                   PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                             UNTIL     IDX-SKJ   >   31
                        MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ(2 IDX-SKJ)
                   END-PERFORM
               END-IF
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
      *    入院会計作成前に会計データの外泊区分を退避
      *    （外泊異動月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SPA-NAI-I01-MAEIDOYMD(1:6)
                                       TO  NACCT-SRYYM
                                           WRK-TENROOM-SKJ-SRYYMX
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        外泊を退避
               MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
               MOVE    SPA-NAI-I01-MAEIDOYMD(7:2)
                                       TO  IDX-SHOKUJIX
               MOVE    SPA-NAI-I01-MAEIDOYMD(7:2)
                                       TO  WRK-TENROOM-SKJ-DD(1)
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   NACCT-DAY(IDX-SKJ)
                                       TO WRK-TENROOM-GAIHAKU(1 IDX-SKJ)
               END-PERFORM
           END-IF
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *    （外泊異動翌月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           IF      WRK-TENROOM-SKJ-SRYMM9  =  12
               ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
               MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
           ELSE
               ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
           END-IF
           MOVE    WRK-TENROOM-SKJ-SRYYMX
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        外泊を退避
               MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(2)
               PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   NACCT-DAY(IDX-SKJ)
                                       TO WRK-TENROOM-GAIHAKU(2 IDX-SKJ)
               END-PERFORM
           END-IF
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    保険組合せ
           MOVE   PTNYUINRRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9     TO  LNK-SCNYUACCT-HKNCOMBINUM
           MOVE   SPA-SRYKA            TO  WRK-TAIHI-SRYKA
           MOVE   "00"                 TO  SPA-SRYKA
      *    入院カレンダー未設定の判定
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
      *    入院カレンダー未設定の判定
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
           MOVE   PTNYUINRRK-BTUNUM   TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
                  DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                            LNK-SCNYUACCT-RC
           END-IF
      *
      *    入院会計作成サブの呼出し（異動前の会計作成）
      *
           MOVE    SPACE               TO  IPN-NYUKAIKEI-SAKUSEI-FLG
      *
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    特定集中治療室管理料算定時に退院日をＳＰＡ−異動日と転入日にセット
      *    以下の特定入院料も同様
      *    救命救急入院料１
      *    救命救急入院料２
      *    一類感染症患者入院医療管理料
      *    広範囲熱傷特定集中治療室管理料
      *    ハイケアユニット入院医療管理料
      *    亜急性期入院医療管理料
      *    脳卒中ケアユニット入院医療管理料
      *    亜急性期入院医療管理料２
      *    亜急性期入院医療管理料１（指定地域）
      *    亜急性期入院医療管理料２（指定地域）
      *    救命救急入院料３（救命救急入院料）
      *    救命救急入院料３（広範囲熱傷特定集中治療）
      *    救命救急入院料４（救命救急入院料）
      *    救命救急入院料４（広範囲熱傷特定集中治療）
      *    特定集中治療室管理料２（特定集中治療室管理料）
      *    特定集中治療室管理料２（広範囲熱傷特定集中治療）
      *    小児特定集中治療室管理料
      *    特定一般病棟入院料（亜急性期入院医療管理）
      *    特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
      *    特定集中治療室管理料１（平成２６年改正）
      *    特定集中治療室管理料２（特定集中治療室管理料）（平成２６年改正）
      *    ハイケアユニット入院医療管理料１（平成２６年改正）
      *    ハイケアユニット入院医療管理料２（平成２６年改正）
      *    地域包括ケア入院医療管理料１（平成２６年改正）
      *    地域包括ケア入院医療管理料１（生活療養）（平成２６年改正）
      *    地域包括ケア入院医療管理料２（平成２６年改正）
      *    地域包括ケア入院医療管理料２（生活療養）（平成２６年改正）
      *    地域包括ケア入院医療管理料１（特定地域）（平成２６年改正）
      *    地域包括ケア入院医療管理料１（生活療養）（特定地域）（平成２６年改正）
      *    地域包括ケア入院医療管理料２（特定地域）（平成２６年改正）
      *    地域包括ケア入院医療管理料２（生活療養）（特定地域）（平成２６年改正）
      *    特定一般病棟入院料（地域包括ケア入院医療管理１）（平成２６年改正）
      *    特定一般病棟入院料（地域包括ケア入院医療管理２）（平成２６年改正）
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN  = "03" OR "04" OR "05" OR
                                                "06" OR "07" OR "08" OR
                                                "09" OR "13" OR "14" OR
                                                "16" OR "17" OR "18" OR
                                                "19" OR "54" OR "55" OR
                                                "56" OR "57" OR "58" OR
                                                "59" OR "60" OR "91" OR
                                                "92" OR "93" OR "95" OR
                                                "96" OR "AH" OR "AI" OR
                                                "AJ" OR "AK" OR
                                                "AL" OR "AM" OR "AN" OR
                                                "AO" OR "AP" OR "AQ" OR
                                                "AR" OR "AS" OR "AU" OR
                                                "AV"
                    MOVE   SPA-NAI-I01-MAEIDOYMD
                                       TO   PTNYUINRRK-TENNYUYMD
                                            SPA-NAI-I01-IDOYMD

               END-IF
           END-IF
      *
      *    緩和ケア病棟入院料、認知症治療病棟入院料１、認知症治療病棟入院料２
      *    を算定時に退院日をＳＰＡ−異動日と転入日にセット
      *    以下の特定入院料も同様
      *    地域包括ケア病棟入院料１（平成２６年改正）
      *    地域包括ケア病棟入院料１（生活療養）（平成２６年改正）
      *    地域包括ケア病棟入院料２（平成２６年改正）
      *    地域包括ケア病棟入院料２（生活療養）（平成２６年改正）
      *    地域包括ケア病棟入院料１（特定地域）（平成２６年改正）
      *    地域包括ケア病棟入院料１（生活療養）（特定地域）（平成２６年改正）
      *    地域包括ケア病棟入院料２（特定地域）（平成２６年改正）
      *    地域包括ケア病棟入院料２（生活療養）（特定地域）（平成２６年改正）
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "2"
              IF    PTNYUINRRK-TOKUTEI-NYUIN  = "09" OR "05" OR "15"
                                             OR "41" OR "42" OR "43"
                                             OR "44" OR "45" OR "46"
                                             OR "47" OR "48"
                    MOVE   SPA-NAI-I01-MAEIDOYMD
                                       TO   PTNYUINRRK-TENNYUYMD
                                            SPA-NAI-I01-IDOYMD
              END-IF
           END-IF
      *
           INITIALIZE      ORCSSYOKIKSNAREA
      *
      *    通算日数、算定開始日を求める
           MOVE    SPA-NAI-I01-MAEIDOYMD  TO   PTNYUINRRK-TENNYUYMD
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( SPA-ERRCD    NOT =  SPACE )
               GO  TO  4801-IDOKAIKEI-CANCEL-EXT
           END-IF
      *
      *    介護病棟からの急性増悪による入院時
           IF      PTNYUINRRK-KAIGOKBN    =    "2"
               COMPUTE    WRK-TSUSAN   =   STUSAN-NISSU01  -  1
           END-IF
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    WRK-SANTEISTYMD
                                   TO  SYOKIKSN-IN-SANTEISTYMD
      *
           MOVE    "5001"          TO  WRK-KANRICD
           MOVE    PTNYUINRRK-BTUNUM
                                   TO  WRK-KBNCD
           MOVE    SPA-NAI-I01-MAEIDOYMD
                                   TO  WRK-YUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
      *     DISPLAY "====AZU  TAIIN-CANCEL=================="
      *     DISPLAY "RRKNUM     = " PTNYUINRRK-RRKNUM
      *     DISPLAY "RRKEDANUM  = " PTNYUINRRK-RRKEDANUM
      *     DISPLAY "NYUINYMD   = " PTNYUINRRK-NYUINYMD
      *     DISPLAY "TAIINYMD   = " PTNYUINRRK-TAIINYMD
      *     DISPLAY "TENNYUYMD  = " PTNYUINRRK-TENNYUYMD
      *     DISPLAY "TENSTUYMD  = " PTNYUINRRK-TENSTUYMD
      *
      *    平成２４年３月３１日時点で亜急性期入院医療管理料１を算定
      *    している状態か判定するサブを呼び出し
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "18"  OR  "54"
                   INITIALIZE                  ORCSAKYUSEI-AREA
                   MOVE    PTNYUINRRK-HOSPNUM  TO  LNK-AKYUSEI-HOSPNUM
                   MOVE    PTNYUINRRK-PTID     TO  LNK-AKYUSEI-PTID
                   MOVE    PTNYUINRRK-RRKNUM   TO  LNK-AKYUSEI-RRKNUM
                   CALL    "ORCSAKYUSEI"    USING  ORCSAKYUSEI-AREA
                                                   SPA-AREA
                   DISPLAY  "LNK-AKYUSEI-RC = " LNK-AKYUSEI-RC
               END-IF
           END-IF
      *
           IF      PTNYUINRRK-TOKUTEI-NYUIN    NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
      *        特定一般病棟入院料の場合は期間加算が存在するため、
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        療養病床で有床診療所入院基本料を相互算定する場合も
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        特定入院料の算定要件外に算定にする以下入院料を追加（平成２６年改正で対応）
      *        ・一般病棟特別入院基本料
      *        ・精神病棟１５対１入院基本料
      *        ・精神病棟特別入院基本料
      *        以下入院料を追加（平成２８年改正で対応）
      *        ・療養病棟・一般病棟７対１入院基本料
      *        ・療養病棟・一般病棟１０対１入院基本料
      *        ・療養病棟・一般病棟１３対１入院基本料
      *        ・療養病棟・一般病棟１５対１入院基本料
      *        以下入院料を追加（平成３０年改正で対応）
      *        ・療養病棟・急性期一般入院料２
      *        ・療養病棟・急性期一般入院料３
      *        ・療養病棟・急性期一般入院料４
      *        ・療養病棟・急性期一般入院料５
      *        ・療養病棟・急性期一般入院料６
      *        ・療養病棟・地域一般入院料１
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"   OR
                                                "099999955"   OR
                                                "099999956"   OR
                                                "099999957"   OR
      *                                         特定入院料の算定要件外の算定
                                                "190079010"   OR
                                                "190083810"   OR
                                                "190085010"   OR
      *                                         平成２８年改正対応
                                                "190198110"   OR
                                                "190198210"   OR
                                                "190198310"   OR
                                                "190198410"   OR
      *                                         平成３０年改正対応
                                                "190201910"   OR
                                                "190202010"   OR
                                                "190202110"   OR
                                                "190202210"   OR
                                                "190202310"   OR
                                                "190202410"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4017"      TO  SPA-ERRCD
                       GO  TO  4801-IDOKAIKEI-CANCEL-EXT
                   END-IF
               END-IF
           ELSE
               IF    ( SPA-NAI-I01-MAEIDOYMD   =  CONST-H180701 )
                AND  ( SYS-5001-BTU-SBT        =  "04"  OR "09" )
                   GO  TO  4801-IDOKAIKEI-CANCEL-EXT
               ELSE
                   IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                       MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                       PERFORM   4801-RJNCD-CHG-SEC
                       PERFORM   4801-YAKINGEN-CHG-SEC
                       CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                       SPA-AREA
                       IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                           MOVE    "4023"      TO  SPA-ERRCD
                           GO  TO  4801-IDOKAIKEI-CANCEL-EXT
                       END-IF
                   ELSE
                       MOVE    "4024"      TO  SPA-ERRCD
                       GO  TO  4801-IDOKAIKEI-CANCEL-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    入院会計作成サブの呼出し
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     "1"     TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                            TO   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                            TO   LNK-SCNYUACCT-STSRYYMD
           PERFORM   VARYING   IDX6   FROM   1   BY   1
               UNTIL (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE) OR
                     (IDX6   >     7)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX6)
           END-PERFORM
      *
      *    医療観察入院セット
           INITIALIZE            ORCSIRYOKNS-AREA
           MOVE     PTNYUINRRK-HOSPNUM   TO  LNK-IRYOKNS-HOSPNUM
           MOVE     PTNYUINRRK-TOKUTEI-KBN
                                         TO  LNK-IRYOKNS-KBN
           MOVE     PTNYUINRRK-TOKUTEI-NYUIN
                                         TO  LNK-IRYOKNS-NYUIN
           MOVE     PTNYUINRRK-BTUNUM    TO  LNK-IRYOKNS-BTUNUM
      *     DISPLAY  "==  ORCSIRYOKNS IDOKAIKEI-CANCEL-CALL ==="
      *     DISPLAY  "SYOKIKSN-IN-SANTEISTYMD = " SYOKIKSN-IN-SANTEISTYMD
      *     DISPLAY  "SPA-NAI-I01-NYUINYMD    = " SPA-NAI-I01-NYUINYMD
           MOVE     SYOKIKSN-IN-SANTEISTYMD(1:6)
                                         TO  LNK-IRYOKNS-SANTEIYM
           CALL     "ORCSIRYOKNS"   USING   ORCSIRYOKNS-AREA
                                            SPA-AREA
           DISPLAY   "LNK-IRYOKNS-RC = " LNK-IRYOKNS-RC
           IF       LNK-IRYOKNS-RC     =    ZERO
               MOVE   LNK-IRYOKNS-SRYCD(1)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   LNK-IRYOKNS-SRYCD(2)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(2)
               MOVE   LNK-IRYOKNS-SRYCD(3)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(3)
               MOVE   LNK-IRYOKNS-SRYCD(4)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(4)
               MOVE   LNK-IRYOKNS-SRYCD(5)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(5)
               MOVE   SYOKIKSN-IN-SANTEISTYMD
                                  TO  LNK-SCNYUACCT-STSRYYMD
                                      LNK-SCNYUACCT-EDSRYYMD
           END-IF
      *
      *    通算算定済日数
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    特定入院で入院料が変更になるコードの対応
      *
      *     DISPLAY "===  TEST IDOKAIKEI-CANCEL ==="
           INITIALIZE                       ORCSTOKNYUIN2-AREA
           MOVE   PTNYUINRRK-HOSPNUM    TO  LNK-TOKNYUIN2-HOSPNUM
           MOVE   PTNYUINRRK-PTID       TO  LNK-TOKNYUIN2-PTID
           MOVE   "1"                   TO  LNK-TOKNYUIN2-IDOKBN
           MOVE   PTNYUINRRK-NYUINYMD   TO  LNK-TOKNYUIN2-NYUINYMD
      *     DISPLAY "SPA-NAI-I01-MAEIDOYMD = " SPA-NAI-I01-MAEIDOYMD
           MOVE   SPA-NAI-I01-MAEIDOYMD TO  LNK-TOKNYUIN2-SANTEIYMD
           MOVE   SYOKIKSN-IN-KHNSRYCD  TO  LNK-TOKNYUIN2-SET-SRYCD
           MOVE   STUSAN-NISSU08        TO  LNK-TOKNYUIN2-NISSU08
           MOVE   PTNYUINRRK-SHONUM     TO  LNK-TOKNYUIN2-SHONUM
           MOVE   PTNYUINRRK-TOKUTEI-NYUIN
                             TO  LNK-TOKNYUIN2-TOKUTEI-NYUIN
           CALL  "ORCSTOKNYUIN2"     USING  ORCSTOKNYUIN2-AREA
                                            SPA-AREA
           IF     LNK-TOKNYUIN2-RC      =   ZERO
              MOVE   SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
              PERFORM   VARYING   IDX    FROM    1   BY  1
                UNTIL   IDX       >    5
                  IF   LNK-TOKNYUIN2-SRYCD(IDX)  NOT =   SPACE
                    MOVE  LNK-TOKNYUIN2-SRYCD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                    MOVE  LNK-TOKNYUIN2-STYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX)
                    MOVE  LNK-TOKNYUIN2-EDYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX)
                  END-IF
              END-PERFORM
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                          TO  IPN-NYUKAIKEI-SAKUSEI-FLG
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                          TO  IPN-SANTEISTYMD
           ELSE
              IF   PTNYUINRRK-TOKUTEI-NYUIN  NOT =   SPACE
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                        TO  IPN-NYUKAIKEI-SAKUSEI-FLG
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                        TO  IPN-SANTEISTYMD
              END-IF
           END-IF
      *
      *    異動取消日が既に特定入院料の上限日を超過している場合は、
      *    点数マスタを参照し、特定入院料であればパラメタをクリアする
           IF      (IPN-NYUKAIKEI-SAKUSEI-FLG   =   "1")   AND
                   (LNK-TOKNYUIN2-SRYCD(1)      =   SPACE)
               INITIALIZE                     TNS-TENSU-REC
               MOVE   PTNYUINRRK-HOSPNUM  TO  TNS-HOSPNUM
               MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                          TO  TNS-SRYCD
               MOVE   IPN-SANTEISTYMD     TO  TNS-YUKOSTYMD
                                              TNS-YUKOEDYMD
               MOVE   TNS-TENSU-REC  TO  MCPDATA-REC
               MOVE   "tbl_tensu"    TO  MCP-TABLE
               MOVE   "key"          TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC        =   ZERO
                   MOVE    MCPDATA-REC TO  TNS-TENSU-REC
                   IF   TNS-NYUTENTTLKBN   =   920
      *                 短期滞在はチェック対象外
                      IF  (TNS-CDKBN-KBN     =   "A")   AND
                          (TNS-CDKBN-KBNNUM  =   400)   AND
                          (TNS-CDKBN-KOUBAN  =   2  OR 3)
                          CONTINUE
                      ELSE
                          MOVE  SPACE TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                      END-IF
                   END-IF
               END-IF
               MOVE    "tbl_tensu"      TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *-------特定入院対応ここまで-----------------------------------
      *
      *    入院料加算診療コード取り込み
           PERFORM    4801-NYUKSN-GET-SEC
      *
           MOVE       1           TO    FLG-IDOKAIKEI-CANCEL
           MOVE      "1"          TO    LNK-SCNYUACCT-IDOSAKUSEI-KBN
      *
      *    異動日に救急・在宅等支援療養病床初期加算（病院）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
           PERFORM   210-KYUKYU-KSN1-SEC
      *
      *    異動日に救急・在宅等支援療養病床初期加算（診療所）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
           PERFORM   210-KYUKYU-KSN2-SEC
      *
      *    異動日に救急・在宅等支援療養病床初期加算（療養病棟入院基本料１）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
           PERFORM   210-KYUKYU-KSN3-SEC
      *
      *    異動日に救急・在宅等支援療養病床初期加算（一般病棟入院基本料）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
           PERFORM   210-KYUKYU-KSN4-SEC
      *
      *    異動日に救急・在宅等支援療養病床初期加算（特定一般病棟入院料）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
           PERFORM   210-KYUKYU-KSN5-SEC
      *
      *    異動日に救急・在宅等支援療養病床初期加算（地域包括ケア病棟入院料）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
           PERFORM   210-KYUKYU-KSN6-SEC
      *
      *    異動日に有床診療所一般病床初期加算が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
           PERFORM   210-YUUSHOUIPPAN-KSN-SEC
      *
      *    異動日に在宅患者支援療養病床初期加算（療養病棟入院基本料）が算定
      *    されているかチェックして、算定されていれば入院料加算の扱いに含める
           PERFORM   210-ZAITAKU-KSN1-SEC
      *
      *    異動日に在宅患者支援病床初期加算（地域包括ケア病棟入院料）が算定
      *    されているかチェックして、算定されていれば入院料加算の扱いに含める
           PERFORM   210-ZAITAKU-KSN2-SEC
      *
           MOVE      ZERO         TO   FLG-IDOKAIKEI-CANCEL
      *
      *    特定入院料区分のセット
           IF      PTNYUINRRK-TOKUTEI-NYUIN    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        特定入院基本料、療養病棟入院基本料、療養病床入院基本料等は
      *        特定入院料扱いから外す
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
      *            特定入院基本料（一般病棟入院基本料）はマスタが
      *            平成２６年９月３０日までの為、参照時の日付を変更
                   IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                       MOVE    "20140930"   TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   END-IF
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
               IF    LNK-SCNYUACCT-KAGEN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *    外泊
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9
                                TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    食事区分
           MOVE   "099999913"   TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           IF     WRK-TAIHI-SHOKUJI    NOT =   ZERO
                  MOVE   WRK-TAIHI-SHOKUJI
                                TO  LNK-SCNYUACCT-SHOKUJIKBN
           ELSE
                  MOVE   1      TO  LNK-SCNYUACCT-SHOKUJIKBN
           END-IF
      *    食堂加算
           PERFORM     4801-BTUSEL-SEC
           IF   FLG-SYSKANRI           =    ZERO
               IF   SYS-5001-BTU-SYOKUDOKBN       =    "2"
                    MOVE     "197000570"    TO
                                LNK-SCNYUACCT-SHOKUDO-SRYCD
                    MOVE    1   TO   LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    保険組合せ
           MOVE   "099999914"   TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           IF     WRK-TAIHI-HKN    NOT =   ZERO
                  MOVE   WRK-TAIHI-HKN
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           ELSE
                  MOVE   PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
                  MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           END-IF
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           IF     PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
      *    入院カレンダー未設定の判定
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え
           IF    (LNK-SCNYUACCT-STSRYYMD   >=   "20120401")  AND
                 (LNK-SCNYUACCT-STSRYYMD   <=   "20140331")
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え（平成２６年改定）
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20140401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169610"
                             MOVE "190170810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169710"
                             MOVE "190170910" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169810"
                             MOVE "190171010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169910"
                             MOVE "190171110" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170010"
                             MOVE "190171210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170110"
                             MOVE "190171310" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170210"
                             MOVE "190171410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170310"
                             MOVE "190171510" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170410"
                             MOVE "190171610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    特定入院基本料のコード細分化置き換え
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161010" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161210" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
      *
      *            特定機能病院は病棟設定された入院コードから判定
                   IF   SYS-5001-BTU-KHNSRYCD =  "190118610" OR
                                                 "190705610" OR
                                                 "190085910"
                     MOVE "190160810" TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-IF
               END-IF
      *        特別入院基本料
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799510"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161110" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161310" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE   PTNYUINRRK-BTUNUM   TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
               DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                         LNK-SCNYUACCT-RC
           END-IF
      *
      *    入院会計作成後に退避データの食事区分を復元
      *   （異動月分）
           IF  (WRK-TENROOM-SKJ-SRYYM(1) >= "200604")   AND
               (WRK-TENROOM-SKJ-SRYYM(1)  NOT =   SPACE)
              PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                 INITIALIZE                      NYUINACCT-REC
                 MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                 MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                 MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                             TO  NACCT-SRYYM
      *
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM  910-NYUINACCT-SELECT-SEC
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM  910-NYUINACCT-READ-SEC
                 IF      FLG-NYUINACCT        =   ZERO
                     MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *              食事を復元
                     EVALUATE  IDX-SKJ-TAIHI
                         WHEN  1
                          PERFORM VARYING IDX-SKJ FROM  1  BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ18(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                         WHEN  2
                          PERFORM VARYING IDX-SKJ FROM  1  BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ19(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                         WHEN  3
                          PERFORM VARYING IDX-SKJ FROM  1  BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ20(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                     END-EVALUATE
      *              剤回数再セット
                     MOVE  ZERO              TO  NACCT-ZAIKAISU
                     PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                              UNTIL     IDX-SKJ   >   31
                         IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                             ADD   1        TO  NACCT-ZAIKAISU
                         END-IF
                     END-PERFORM   
                 END-IF
                 MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                 MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                 MOVE     "key"              TO  MCP-PATHNAME
                 PERFORM 910-DBUPDATE-SEC
      *          入院会計マスタクローズ
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM 910-DBCLOSECURSOR-SEC
              END-PERFORM
           END-IF
      *   （異動翌月分）
           IF  (WRK-TENROOM-SKJ-SRYYM(2) >= "200604")   AND
               (WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE)
              PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                   MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                               TO  NACCT-SRYYM
      *
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM  910-NYUINACCT-SELECT-SEC
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM  910-NYUINACCT-READ-SEC
                   IF      FLG-NYUINACCT        =   ZERO
                       MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *                食事を復元
                       EVALUATE  IDX-SKJ-TAIHI
                           WHEN  1
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ18(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                           WHEN  2
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ19(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                           WHEN  3
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ20(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                       END-EVALUATE
      *                剤回数再セット
                       MOVE  ZERO              TO  NACCT-ZAIKAISU
                       PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                           IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1        TO  NACCT-ZAIKAISU
                           END-IF
                       END-PERFORM   
                   END-IF
                   MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                   MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                   MOVE     "key"              TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
      *            入院会計マスタクローズ
                   MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM 910-DBCLOSECURSOR-SEC
              END-PERFORM
           END-IF
      *   （異動月分）
           IF  (WRK-TENROOM-SKJ-SRYYM(1) < "200603")   AND
               (WRK-TENROOM-SKJ-SRYYM(1)  NOT =   SPACE)
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            食事を復元
                   MOVE    WRK-TENROOM-SKJ-DD(1)
                                       TO  IDX-SHOKUJIX
                   PERFORM  VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                            UNTIL     IDX-SKJ   >   31
                       MOVE   WRK-TENROOM-SKJ(1 IDX-SKJ)
                                           TO  NACCT-DAY(IDX-SKJ)
                   END-PERFORM
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *   （異動翌月分）
           IF  (WRK-TENROOM-SKJ-SRYYM(2) < "200603")   AND
               (WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE)
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            食事を復元
                   PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                             UNTIL     IDX-SKJ   >   31
                        MOVE   WRK-TENROOM-SKJ(2 IDX-SKJ)
                                           TO  NACCT-DAY(IDX-SKJ)
                   END-PERFORM
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *    入院会計作成後に退避データの外泊区分を復元
      *    （外泊異動月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        外泊を復元
               MOVE    WRK-TENROOM-SKJ-DD(1)
                                       TO  IDX-SHOKUJIX
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   WRK-TENROOM-GAIHAKU(1 IDX-SKJ)
                                       TO  NACCT-DAY(IDX-SKJ)
               END-PERFORM
           END-IF
           MOVE     NYUINACCT-REC      TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *    （異動翌月分）
           IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM   910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            外泊を復元
                   PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                             UNTIL     IDX-SKJ   >   31
                        MOVE   WRK-TENROOM-GAIHAKU(2 IDX-SKJ)
                                           TO  NACCT-DAY(IDX-SKJ)
                   END-PERFORM
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *    入院会計作成後に食事の算定が無い日の食堂加算をクリアする
      *    処理を追加
           IF  SPA-NAI-I01-MAEIDOYMD(1:6)  >= "200604"
               MOVE      SPACE             TO   WRK-SHOKUDOU-SRYYM
               IF        WRK-TENROOM-SKJ-SRYYM(1)   NOT =   SPACE
                   MOVE  WRK-TENROOM-SKJ-SRYYM(1)
                                           TO   WRK-SHOKUDOU-SRYYM
                   PERFORM   4801-SHOKUDOU-CLEAR-SEC
               END-IF
               MOVE      SPACE             TO   WRK-SHOKUDOU-SRYYM
               IF        WRK-TENROOM-SKJ-SRYYM(2)   NOT =   SPACE
                   MOVE  WRK-TENROOM-SKJ-SRYYM(2)
                                           TO   WRK-SHOKUDOU-SRYYM
                   PERFORM   4801-SHOKUDOU-CLEAR-SEC
               END-IF
           END-IF
      *
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
               MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
               MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
               MOVE    IPN-SANTEISTYMD   TO   SPA-NAI-I01-IDOYMD
               MOVE    IPN-SANTEISTYMD   TO   PTNYUINRRK-TENNYUYMD
               IF      PTNYUINRRK-SKJKBN  =   "B"  OR  "C"
                   CONTINUE
               ELSE
                   MOVE    "A"               TO   PTNYUINRRK-SKJKBN
               END-IF
               PERFORM   4801-TENROOM-EDIT-SEC
           END-IF
      *
           .
       4801-IDOKAIKEI-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *     選定療養費診療行為コード取得処理
      *****************************************************************
       4801-SENTEI-SET-SEC         SECTION.
      *
           INITIALIZE  SENTEICDCHG-REC
           MOVE    LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                   TO  SENCHG-IPNSRYCD
           MOVE    SENTEICDCHG-REC TO  MCPDATA-REC
           MOVE    "tbl_senteicdchg"   TO MCP-TABLE
           MOVE    "key2"              TO MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    MCPDATA-REC TO  SENTEICDCHG-REC
           ELSE
               INITIALIZE              SENTEICDCHG-REC
           END-IF
      *
           MOVE    "tbl_senteicdchg"   TO MCP-TABLE
           MOVE    "key2"              TO MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      **     MOVE    SENCHG-SENTEISRYCD
      **                             TO  LNK-SCNYUACCT-SENTEI-SRYCD
           IF      SENCHG-SENTEISRYCD    NOT =  SPACE
               MOVE    "099999916"   TO  LNK-SCNYUACCT-SENTEI-SRYCD
               MOVE    WRK-SENTEI-STYMD
                                     TO  LNK-SCNYUACCT-SENTEI-STYMD
           END-IF
      *    １５才未満の患者は選定入院対象外
           MOVE     PTINF-BIRTHDAY   TO  WRK-BIRTHDAY-15
           ADD      15               TO  WRK-BIRTHDAY-15Y
           IF       WRK-SENTEI-STYMD <=  WRK-BIRTHDAY-15
               MOVE   WRK-BIRTHDAY-15  TO  LNK-SCNYUACCT-SENTEI-STYMD
           END-IF
      *
      *
           .
       4801-SENTEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    通算日数取得処理
      *****************************************************************
       4801-TSUSAN-SANTEI-GET-SEC      SECTION.
      *
           INITIALIZE                  WRK-TSUSAN
                                       WRK-SANTEISTYMD
      *
           INITIALIZE                  ORCSTUSANAREA
           MOVE    SPA-HOSPNUM      TO  STUSAN-HOSPNUM
           MOVE    PTNYUINRRK-PTID TO  STUSAN-PTID
      *---------------退院日の同日入院処理ここから----------------------
           IF   WRK-DOU-TENNYUYMD    NOT =   SPACE
               MOVE    WRK-DOU-TENNYUYMD  TO  PTNYUINRRK-TENNYUYMD
           END-IF
      *---------------退院日の同日入院処理ここまで----------------------
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  STUSAN-KJNYMD
           DISPLAY "STUSAN-HOSPNUM =" STUSAN-HOSPNUM
           DISPLAY "STUSAN-PTID    =" STUSAN-PTID
           DISPLAY "STUSAN-KJNYMD  =" STUSAN-KJNYMD
      *    特定入院での算定済日数を取得用
      *    特定集中治療室管理料
      *    救命救急入院料１
      *    救命救急入院料１（高度）
      *    救命救急入院料１（Ａ）
      *    救命救急入院料２
      *    救命救急入院料２（高度）
      *    救命救急入院料２（Ａ）
      *    一類感染症患者入院医療管理料
      *    広範囲熱傷特定集中治療室管理料
      *    ハイケアユニット入院医療管理料
      *    亜急性期入院医療管理料
      *    脳卒中ケアユニット入院医療管理料
      *    亜急性期入院医療管理料２
      *    亜急性期入院医療管理料１（指定地域）
      *    亜急性期入院医療管理料２（指定地域）
      *    救命救急入院料３（救命救急入院料）
      *    救命救急入院料３（広範囲熱傷特定集中治療）
      *    救命救急入院料４（救命救急入院料）
      *    救命救急入院料４（広範囲熱傷特定集中治療）
      *    特定集中治療室管理料２（特定集中治療室管理料）
      *    特定集中治療室管理料２（広範囲熱傷特定集中治療）
      *    小児特定集中治療室管理料
      *    特定一般病棟入院料（亜急性期入院医療管理）
      *    特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               EVALUATE   PTNYUINRRK-TOKUTEI-NYUIN
                   WHEN   "03"
                       MOVE   "190024510"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190128610"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190074510"  TO  STUSAN-TOKUTEI-SRYCD(3)
      *                （栄管経過措置）
                       MOVE   "190714410"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190714510"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190714610"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "04"
                       MOVE   "190110210"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190129110"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190110610"  TO  STUSAN-TOKUTEI-SRYCD(3)
                   WHEN   "05"
                       MOVE   "190110310"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190128910"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190110710"  TO  STUSAN-TOKUTEI-SRYCD(3)
                   WHEN   "06"
                       MOVE   "190024310"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190128710"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190024410"  TO  STUSAN-TOKUTEI-SRYCD(3)
      *                （栄管経過措置）
                       MOVE   "190714710"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190714810"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190714910"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "07"
                       MOVE   "190110410"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190129210"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190110810"  TO  STUSAN-TOKUTEI-SRYCD(3)
                   WHEN   "08"
                       MOVE   "190110510"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190129010"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190110910"  TO  STUSAN-TOKUTEI-SRYCD(3)
                   WHEN   "09"
                   WHEN   "16"
                       MOVE   "190024610"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190074610"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190116310"  TO  STUSAN-TOKUTEI-SRYCD(3)
                       MOVE   "190116410"  TO  STUSAN-TOKUTEI-SRYCD(4)
      *                （栄管経過措置）
                       MOVE   "190717210"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190717310"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "14"
                       MOVE   "190075510"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190075610"  TO  STUSAN-TOKUTEI-SRYCD(2)
      *                （栄管経過措置）
                       MOVE   "190718710"  TO  STUSAN-TOKUTEI-SRYCD(3)
                       MOVE   "190718810"  TO  STUSAN-TOKUTEI-SRYCD(4)
      *                （平成２８年度コード変更）
                       MOVE   "190194310"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190194410"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "13"
                       MOVE   "190024810"  TO  STUSAN-TOKUTEI-SRYCD(1)
                   WHEN   "17"
                       MOVE   "190117310"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                （栄管経過措置）
                       MOVE   "190717810"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "18"
                       MOVE   "190117410"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                （栄管経過措置）
                       MOVE   "190720110"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "19"
                       MOVE   "190120810"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                （栄管経過措置）
                       MOVE   "190717910"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "54"
                       MOVE   "190129910"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                （栄管経過措置）
                       MOVE   "190720210"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "55"
                       MOVE   "190138110"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190138210"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190138310"  TO  STUSAN-TOKUTEI-SRYCD(3)
      *                （栄管経過措置）
                       MOVE   "190715010"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190715110"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190715210"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "56"
                       MOVE   "190138410"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190138510"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190138610"  TO  STUSAN-TOKUTEI-SRYCD(3)
      *                （栄管経過措置）
                       MOVE   "190715310"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190715410"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190715510"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "57"
                       MOVE   "190138710"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190138810"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190138910"  TO  STUSAN-TOKUTEI-SRYCD(3)
      *                （栄管経過措置）
                       MOVE   "190715610"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190715710"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190715810"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "58"
                       MOVE   "190139010"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190139110"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190139210"  TO  STUSAN-TOKUTEI-SRYCD(3)
                       MOVE   "190139310"  TO  STUSAN-TOKUTEI-SRYCD(4)
      *                （栄管経過措置）
                       MOVE   "190715910"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190716010"  TO  STUSAN-TOKUTEI-SRYCD(6)
                       MOVE   "190716110"  TO  STUSAN-TOKUTEI-SRYCD(7)
                       MOVE   "190716210"  TO  STUSAN-TOKUTEI-SRYCD(8)
                   WHEN   "59"
                       MOVE   "190139810"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190139910"  TO  STUSAN-TOKUTEI-SRYCD(2)
      *                （栄管経過措置）
                       MOVE   "190717410"  TO  STUSAN-TOKUTEI-SRYCD(3)
                       MOVE   "190717510"  TO  STUSAN-TOKUTEI-SRYCD(4)
                   WHEN   "60"
                       MOVE   "190140010"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190140110"  TO  STUSAN-TOKUTEI-SRYCD(2)
      *                （栄管経過措置）
                       MOVE   "190717610"  TO  STUSAN-TOKUTEI-SRYCD(3)
                       MOVE   "190717710"  TO  STUSAN-TOKUTEI-SRYCD(4)
                   WHEN   "91"
                       MOVE   "190149910"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190150010"  TO  STUSAN-TOKUTEI-SRYCD(2)
      *                （栄管経過措置）
                       MOVE   "190718010"  TO  STUSAN-TOKUTEI-SRYCD(3)
                       MOVE   "190718110"  TO  STUSAN-TOKUTEI-SRYCD(4)
                   WHEN   "92"
                       MOVE   "190150310"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                （栄管経過措置）
                       MOVE   "190720310"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "93"
                       MOVE   "190150410"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                （栄管経過措置）
                       MOVE   "190720410"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "95"
                       MOVE   "190152610"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                （栄管経過措置）
                       MOVE   "190723110"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "96"
                       MOVE   "190152710"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                （栄管経過措置）
                       MOVE   "190723210"  TO  STUSAN-TOKUTEI-SRYCD(2)
               END-EVALUATE
           END-IF
      *    精神科急性期治療病棟入院料１
      *    精神科急性期治療病棟入院料２
      *    精神科急性期治療病棟入院料１（急性増悪の転棟）
      *    精神科急性期治療病棟入院料２（急性増悪の転棟）
      *    認知症治療病棟入院料１
      *    認知症治療病棟入院料２
      *    緩和ケア病棟入院料
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "2"
               EVALUATE   PTNYUINRRK-TOKUTEI-NYUIN
                   WHEN   "11"
                       MOVE   "190121110"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                （栄管経過措置）
                       MOVE   "190721410"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "12"
                       MOVE   "190121210"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                （栄管経過措置）
                       MOVE   "190721610"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "24"
                       MOVE   "190121110"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                （栄管経過措置）
                       MOVE   "190721410"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "25"
                       MOVE   "190121210"  TO  STUSAN-TOKUTEI-SRYCD(1)
      *                （栄管経過措置）
                       MOVE   "190721610"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "05"
                       MOVE   "190151610"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190739910"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190740010"  TO  STUSAN-TOKUTEI-SRYCD(3)
      *                （栄管経過措置）
                       MOVE   "190722310"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190722410"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190722510"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "15"
                       MOVE   "190151710"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190813410"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190813510"  TO  STUSAN-TOKUTEI-SRYCD(3)
      *                （栄管経過措置）
                       MOVE   "190722610"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190722710"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190722810"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "09"
                       MOVE   "190028910"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190150510"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190150610"  TO  STUSAN-TOKUTEI-SRYCD(3)
      *                （栄管経過措置）
                       MOVE   "190720710"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190720810"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190720910"  TO  STUSAN-TOKUTEI-SRYCD(6)
               END-EVALUATE
           END-IF
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           DISPLAY "STUSAN-RC=" STUSAN-RC
           IF    ( STUSAN-RC       NOT =   ZERO )
               MOVE    "4025"      TO  SPA-ERRCD
               GO  TO  4801-TSUSAN-SANTEI-GET-EXT
           END-IF
      *
      *    他院（特別な関係にある医療機関）の日数を加算
           ADD     STUSAN-TAINREL-NISSU  TO   STUSAN-NISSU03
           ADD     STUSAN-TAINREL-NISSU  TO   STUSAN-NISSU05
      *
           COMPUTE WRK-TSUSAN  =   STUSAN-NISSU03  -   1
      *---------------退院日の同日入院処理ここから----------------------
      *     IF   WRK-DOU-TENNYUYMD    NOT =   SPACE
      *         MOVE    WRK-DOU-TENNYUYMD  TO  PTNYUINRRK-TENNYUYMD
      *     END-IF
      *---------------退院日の同日入院処理ここまで----------------------
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-SANTEISTYMD
      *
           MOVE    WRK-SANTEISTYMD
                                   TO  WRK-YMD
           MOVE    "1"             TO  WRK-ZOGENPTN
           IF    ( STUSAN-KJNYMD-CHK-FLG  =   ZERO )
               COMPUTE WRK-ZOGEN   =   180 -   STUSAN-NISSU04
           ELSE
               COMPUTE WRK-ZOGEN   =   180 - ( STUSAN-NISSU04  -   1 )
           END-IF
      *
           PERFORM 5002-CALENDAR-SEC
           MOVE    WRK-KEISANYMD   TO  WRK-SENTEI-STYMD
      *
           .
       4801-TSUSAN-SANTEI-GET-EXT.
           EXIT.
      ******************************************************************
      *    通算日数、算定開始日取得処理（入院登録時）
      *****************************************************************
      *- 4801-NYUTSUSAN-SANTEI-GET-SEC   SECTION.
      *-*
      *-     INITIALIZE                  WRK-TSUSAN
      *-                                 WRK-SANTEISTYMD
      *-*
      *-     PERFORM VARYING IDX1    FROM    1   BY  1
      *-         UNTIL     ( IDX1    >  CONST-NYUREKILST-MAX )
      *-         IF    ( SPA-NAI-I01-TZITA-FLG (IDX1)
      *-                                             =   2 )
      *-          AND  ( SPA-GMN-I01-TSHONUM (IDX1)
      *-                             =   PTNYUINRRK-SHONUM )
      *-             COMPUTE WRK-TSUSAN
      *-                 =   WRK-TSUSAN
      *-                 +   SPA-NAI-I01-TNYURRKTAISYO (IDX1)
      *-         END-IF
      *-*
      *-     END-PERFORM
      *-*
      *-*    歴をさかのぼり通算の対象となるデータを抽出する
      *-     INITIALIZE    WRK-REKI-ZIIN-TBL
      *-     PERFORM VARYING IDX1    FROM    1   BY  1
      *-         UNTIL     ( IDX1    >  CONST-NYUREKILST-MAX )
      *-         IF    ( SPA-NAI-I01-TZITA-FLG (IDX1)
      *-                                             =   ZERO )
      *-          AND  ( SPA-GMN-I01-TSHONUM (IDX1)
      *-                             =   PTNYUINRRK-SHONUM )
      *-*
      *-             COMPUTE WRK-REKI-ZIIN-CNT
      *-                 =   WRK-REKI-ZIIN-CNT   +   1
      *-*
      *-             MOVE    WRK-REKI-ZIIN-CNT   TO  IDX2
      *-             MOVE    SPA-NAI-I01-TNYUINYMD (IDX1)
      *-                             TO  WRK-REKI-ZIIN-NYUINYMD (IDX2)
      *-             MOVE    SPA-NAI-I01-TTAIINYMD (IDX1)
      *-                             TO  WRK-REKI-ZIIN-TAIINYMD (IDX2)
      *-*
      *-            IF    ( SPA-GMN-I01-TSHOKAI (IDX1)  =   "1" )
      *-                MOVE   CONST-NYUREKILST-MAX     TO  IDX1
      *-            END-IF
      *-*
      *-         END-IF
      *-     END-PERFORM
      *-*
      *-*    入院日が前回退院日と同日の場合は、翌日を算定開始日に設定する
      *-     IF    ( SPA-GMN-I01-NYUREKILST-CNT  >   ZERO )
      *-      AND  ( SPA-NAI-I01-TZITA-FLG (1)   =   ZERO )
      *-      AND  ( PTNYUINRRK-NYUINYMD
      *-                             =   SPA-NAI-I01-TTAIINYMD (1) )
      *-         MOVE    PTNYUINRRK-NYUINYMD
      *-                             TO WRK-YMD
      *-         MOVE    "1"         TO WRK-ZOGENPTN
      *-         MOVE    1           TO WRK-ZOGEN
      *-         PERFORM 5002-CALENDAR-SEC
      *-         MOVE    WRK-KEISANYMD
      *-                             TO  WRK-SANTEISTYMD
      *-     ELSE
      *-         MOVE    PTNYUINRRK-NYUINYMD
      *-                             TO  WRK-SANTEISTYMD
      *-     END-IF
      *-*
      *-*    今回が初回の場合、または過去に歴作成データしかない場合は、
      *-*    以下の処理は行わない
      *-     IF    ( PTNYUINRRK-SHOKAIKBN    =   "1" )
      *-       OR  ( WRK-REKI-ZIIN-CNT      <=  ZERO )
      *-     DISPLAY "************************"
      *-     DISPLAY "*** NYUTSUSAN-SANTEI ***"
      *-     DISPLAY "WRK-TSUSAN      = " WRK-TSUSAN
      *-     DISPLAY "WRK-SANTEISTYMD = " WRK-SANTEISTYMD
      *-     DISPLAY "************************"
      *-         GO  TO  4801-NYUTSUSAN-SANTEI-GET-EXT
      *-     END-IF
      *-*
      *-     PERFORM VARYING IDX1
      *-             FROM    WRK-REKI-ZIIN-CNT
      *-             BY      -1
      *-             UNTIL ( IDX1    <   1 )
      *-*
      *-         MOVE    WRK-REKI-ZIIN-NYUINYMD (IDX1)
      *-                         TO  WRK-STYMD
      *-         MOVE    WRK-REKI-ZIIN-TAIINYMD (IDX1)
      *-                         TO  WRK-EDYMD
      *-         PERFORM 5002-KIKAN-CALC-SEC
      *-         IF    ( FLG-ORCSDAY =   ZERO )
      *-             IF    ( WRK-REKI-ZIIN-CNT
      *-                                     >   IDX1 )
      *-              AND  ( WRK-REKI-ZIIN-NYUINYMD (IDX1)
      *-                 =   WRK-REKI-ZIIN-TAIINYMD (IDX1 + 1) )
      *-                 COMPUTE WRK-TSUSAN
      *-                     =   WRK-TSUSAN  +   WRK-NISSU2  -   1
      *-             ELSE
      *-                 COMPUTE WRK-TSUSAN
      *-                     =   WRK-TSUSAN  +   WRK-NISSU2
      *-             END-IF
      *-         END-IF
      *-*
      *-     END-PERFORM
      *-*
      *-     DISPLAY "*** NYUTSUSAN-SANTEI ***"
      *-     DISPLAY "WRK-TSUSAN = "  WRK-TSUSAN
      *-     DISPLAY "WRK-SANTEISTYMD = " WRK-SANTEISTYMD
      *-     DISPLAY "************************"
      *-*
      *-     .
      *- 4801-NYUTSUSAN-SANTEI-GET-EXT.
      *-     EXIT.
      *****************************************************************
      *    通算日数、算定開始日取得処理（転科・転棟・転室時）
      *****************************************************************
      *- 4801-IDOTSUSAN-SANTEI-GET-SEC   SECTION.
      *-*
      *-     INITIALIZE                  WRK-NYUINNISSU
      *-                                 WRK-TSUSAN
      *-                                 WRK-SANTEISTYMD
      *-*
      *-     PERFORM VARYING IDX1    FROM    1   BY  1
      *-         UNTIL     ( IDX1    >  CONST-NYUREKILST-MAX )
      *-         IF    ( SPA-NAI-I01-TZITA-FLG (IDX1)
      *-                                             =   2 )
      *-          AND  ( SPA-GMN-I01-TSHONUM (IDX1)
      *-                             =   PTNYUINRRK-SHONUM )
      *-             COMPUTE WRK-TSUSAN
      *-                 =   WRK-TSUSAN
      *-                 +   SPA-NAI-I01-TNYURRKTAISYO (IDX1)
      *-         END-IF
      *-*
      *-     END-PERFORM
      *-*
      *-*    入院日から（異動日当日を含まない）異動日までの日数を求める
      *-*    （入院日数とする）
      *-     MOVE    PTNYUINRRK-NYUINYMD
      *-                                 TO  WRK-STYMD
      *-     MOVE    PTNYUINRRK-TENNYUYMD
      *-                                 TO  WRK-EDYMD
      *-     PERFORM 5002-KIKAN-CALC-SEC
      *-     MOVE    WRK-NISSU1          TO  WRK-NYUINNISSU
      *-*
      *-*    歴をさかのぼり通算の対象となるデータを抽出する
      *-     INITIALIZE    WRK-REKI-ZIIN-TBL
      *-     PERFORM VARYING IDX1    FROM    2   BY  1
      *-         UNTIL     ( IDX1    >  CONST-NYUREKILST-MAX )
      *-         IF    ( SPA-NAI-I01-TZITA-FLG (IDX1)
      *-                                             =   ZERO )
      *-          AND  ( SPA-GMN-I01-TSHONUM (IDX1)
      *-                             =   PTNYUINRRK-SHONUM )
      *-*
      *-             COMPUTE WRK-REKI-ZIIN-CNT
      *-                 =   WRK-REKI-ZIIN-CNT   +   1
      *-*
      *-             MOVE    WRK-REKI-ZIIN-CNT   TO  IDX2
      *-             MOVE    SPA-NAI-I01-TNYUINYMD (IDX1)
      *-                             TO  WRK-REKI-ZIIN-NYUINYMD (IDX2)
      *-             MOVE    SPA-NAI-I01-TTAIINYMD (IDX1)
      *-                             TO  WRK-REKI-ZIIN-TAIINYMD (IDX2)
      *-*
      *-            IF    ( SPA-GMN-I01-TSHOKAI (IDX1)  =   "1" )
      *-                MOVE  CONST-NYUREKILST-MAX      TO  IDX1
      *-            END-IF
      *-*
      *-         END-IF
      *-     END-PERFORM
      *-*
      *-*    異動日が前回退院日と同日の場合は、翌日を算定開始日に設定する
      *-     IF    ( SPA-GMN-I01-NYUREKILST-CNT  >   1 )
      *-      AND  ( SPA-NAI-I01-TZITA-FLG (2)   =   ZERO )
      *-      AND  ( PTNYUINRRK-TENNYUYMD
      *-                             =   SPA-NAI-I01-TTAIINYMD (2) )
      *-         MOVE    PTNYUINRRK-TENNYUYMD
      *-                             TO WRK-YMD
      *-         MOVE    "1"         TO WRK-ZOGENPTN
      *-         MOVE    1           TO WRK-ZOGEN
      *-         PERFORM 5002-CALENDAR-SEC
      *-         MOVE    WRK-KEISANYMD
      *-                             TO  WRK-SANTEISTYMD
      *-     ELSE
      *-*        異動日が前回退院日と同日で無い場合は、異動日を算定開始日
      *-*        に設定する。
      *-*        通算日数は入院日と前回退院日が同日の場合は、
      *-*        歴を遡って求めた通算日数＋入院日数−１
      *-*        そうでない場合、通算日数＋入院日数を通算日数に設定する
      *-         MOVE    PTNYUINRRK-TENNYUYMD
      *-                             TO  WRK-SANTEISTYMD
      *-         IF    ( SPA-GMN-I01-NYUREKILST-CNT  >   1 )
      *-          AND  ( SPA-NAI-I01-TZITA-FLG (2)   =   ZERO )
      *-          AND  ( PTNYUINRRK-NYUINYMD
      *-                             =   SPA-NAI-I01-TTAIINYMD (2) )
      *-             COMPUTE WRK-TSUSAN
      *-                 =   WRK-TSUSAN  +   WRK-NYUINNISSU  -   1
      *-         ELSE
      *-             COMPUTE WRK-TSUSAN
      *-                 =   WRK-TSUSAN  +   WRK-NYUINNISSU
      *-         END-IF
      *-     END-IF
      *-*
      *-*    今回が初回の場合、または過去に歴作成データしかない場合は、
      *-*    以下の処理は行わない
      *-     IF    ( PTNYUINRRK-SHOKAIKBN    =   "1" )
      *-       OR  ( WRK-REKI-ZIIN-CNT      <=  ZERO )
      *-     DISPLAY "************************"
      *-     DISPLAY "*** IDOTSUSAN-SANTEI ***"
      *-     DISPLAY "WRK-TSUSAN      = " WRK-TSUSAN
      *-     DISPLAY "WRK-SANTEISTYMD = " WRK-SANTEISTYMD
      *-     DISPLAY "************************"
      *-         GO  TO  4801-IDOTSUSAN-SANTEI-GET-EXT
      *-     END-IF
      *-*
      *-     PERFORM VARYING IDX1
      *-             FROM    WRK-REKI-ZIIN-CNT
      *-             BY      -1
      *-             UNTIL ( IDX1    <   1 )
      *-*
      *-         MOVE    WRK-REKI-ZIIN-NYUINYMD (IDX1)
      *-                         TO  WRK-STYMD
      *-         MOVE    WRK-REKI-ZIIN-TAIINYMD (IDX1)
      *-                         TO  WRK-EDYMD
      *-         PERFORM 5002-KIKAN-CALC-SEC
      *-         IF    ( FLG-ORCSDAY =   ZERO )
      *-             IF    ( WRK-REKI-ZIIN-CNT
      *-                                     >   IDX1 )
      *-              AND  ( WRK-REKI-ZIIN-NYUINYMD (IDX1)
      *-                 =   WRK-REKI-ZIIN-TAIINYMD (IDX1 + 1) )
      *-                 COMPUTE WRK-TSUSAN
      *-                     =   WRK-TSUSAN  +   WRK-NISSU2  -   1
      *-             ELSE
      *-                 COMPUTE WRK-TSUSAN
      *-                     =   WRK-TSUSAN  +   WRK-NISSU2
      *-             END-IF
      *-         END-IF
      *-*
      *-     END-PERFORM
      *-*
      *-     DISPLAY "*** IDOTSUSAN-SANTEI ***"
      *-     DISPLAY "WRK-TSUSAN = "  WRK-TSUSAN
      *-     DISPLAY "WRK-SANTEISTYMD = " WRK-SANTEISTYMD
      *-     DISPLAY "************************"
      *-*
      *-     .
      *- 4801-IDOTSUSAN-SANTEI-GET-EXT.
      *-     EXIT.
      *****************************************************************
      *    病棟情報検索処理
      *****************************************************************
       4801-BTUSEL-SEC             SECTION.
      *
           MOVE    "5001"          TO  WRK-KANRICD
           MOVE    PTNYUINRRK-BTUNUM
                                   TO  WRK-KBNCD
           EVALUATE   SPA-GMN-I01-SYORIKBN-CD
           WHEN   "01"
               MOVE   SPA-NAI-I01-NYUINYMD
                                   TO  WRK-YUKYMD
           WHEN   "07"
               MOVE   SPA-NAI-I01-TAIINYMD
                                   TO  WRK-YUKYMD
           WHEN   "08"
               MOVE   SPA-NAI-I01-IDOYMD
                                   TO  WRK-YUKYMD
           WHEN   "09"
               MOVE   SPA-NAI-I01-MAEIDOYMD
                                   TO  WRK-YUKYMD
           WHEN   OTHER
               MOVE    SPA-SYSYMD
                                   TO  WRK-YUKYMD
           END-EVALUATE
      *
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
           .
       4801-BTUSEL-EXT.
           EXIT.
      *****************************************************************
      *    病室情報検索処理
      *****************************************************************
       4801-BRMSEL-SEC             SECTION.
      *
           MOVE    "5002"          TO  WRK-KANRICD
           MOVE    PTNYUINRRK-BRMNUM
                                   TO  WRK-KBNCD
           EVALUATE   SPA-GMN-I01-SYORIKBN-CD
           WHEN   "01"
               MOVE   SPA-NAI-I01-NYUINYMD
                                   TO  WRK-YUKYMD
           WHEN   "07"
               MOVE   SPA-NAI-I01-TAIINYMD
                                   TO  WRK-YUKYMD
           WHEN   "08"
               MOVE   SPA-NAI-I01-IDOYMD
                                   TO  WRK-YUKYMD
           WHEN   "09"
               MOVE   SPA-NAI-I01-MAEIDOYMD
                                   TO  WRK-YUKYMD
           WHEN   OTHER
               MOVE    SPA-SYSYMD
                                   TO  WRK-YUKYMD
           END-EVALUATE
      *
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5002-REC
      *
           .
       4801-BRMSEL-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料セット処理
      *****************************************************************
       4801-TOKTEI-NYUIN-SET-SEC   SECTION.
      *
      *    特定入院料コードをサブプロより取得
           INITIALIZE                           ORCSTOKNYUIN-AREA
           MOVE    PTNYUINRRK-TOKUTEI-KBN   TO  LNK-TOKNYUIN-KBN
           MOVE    PTNYUINRRK-TOKUTEI-NYUIN TO  LNK-TOKNYUIN-NYUIN
           MOVE    PTNYUINRRK-BTUNUM        TO  LNK-TOKNYUIN-BTUNUM
           MOVE    PTNYUINRRK-TOKU-SANTEIYOUKENKBN
                                        TO  LNK-TOKNYUIN-SANTEIYOUKENKBN
           MOVE    SYOKIKSN-IN-SANTEISTYMD(1:6)
                                            TO  LNK-TOKNYUIN-SANTEIYM
           CALL    "ORCSTOKNYUIN"      USING    ORCSTOKNYUIN-AREA
                                                SPA-AREA
           IF      LNK-TOKNYUIN-RC          =   ZERO
               IF    LNK-TOKNYUIN-GAI-SRYCD =  SPACE
                   MOVE   LNK-TOKNYUIN-SRYCD   TO  SYOKIKSN-IN-KHNSRYCD
               ELSE
                   MOVE   LNK-TOKNYUIN-GAI-SRYCD
                                               TO  SYOKIKSN-IN-KHNSRYCD
               END-IF
           END-IF
      *
           .
       4801-BTU-TOKTEI-NYUIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    夜勤時間超過減算コードへの置き換え処理
      *    夜勤時間特別入院基本料コードへの置き換え処理
      *    看護職員数等入院基本料コードへの置き換え処理
      *****************************************************************
       4801-YAKINGEN-CHG-SEC   SECTION.
      *
           IF    SYOKIKSN-IN-SANTEISTYMD   >=   "20140401"
      *        夜勤時間超過減算入院料コード取得
               INITIALIZE    ORCSYAKINGEN-AREA
               MOVE   SYOKIKSN-IN-SANTEISTYMD(1:6)
                                   TO    LNK-YAKINGEN-SANTEIYM
               MOVE   SYS-5001-BTU-NUM
                                   TO    LNK-YAKINGEN-BTU-NUM
               MOVE   SYOKIKSN-IN-KHNSRYCD
                                   TO    LNK-YAKINGEN-SRYCD
               CALL   "ORCSYAKINGEN"    USING   ORCSYAKINGEN-AREA
                                                SPA-AREA
               IF   LNK-YAKINGEN-RC     =       ZERO
                    MOVE LNK-YAKINGEN-CHGSRYCD
                                   TO    SYOKIKSN-IN-KHNSRYCD
               END-IF
           END-IF
           IF    SYOKIKSN-IN-SANTEISTYMD   >=   "20160401"
      *        夜勤時間特別入院基本料コード取得
               INITIALIZE    ORCSYAKINTOKU-AREA
               MOVE   SYOKIKSN-IN-SANTEISTYMD(1:6)
                                   TO    LNK-YAKINTOKU-SANTEIYM
               MOVE   SYS-5001-BTU-NUM
                                   TO    LNK-YAKINTOKU-BTU-NUM
               MOVE   SYOKIKSN-IN-KHNSRYCD
                                   TO    LNK-YAKINTOKU-SRYCD
               CALL   "ORCSYAKINTOKU"    USING  ORCSYAKINTOKU-AREA
                                                SPA-AREA
               IF   LNK-YAKINTOKU-RC     =       ZERO
                    MOVE LNK-YAKINTOKU-CHGSRYCD
                                   TO    SYOKIKSN-IN-KHNSRYCD
               END-IF
      *        看護職員数等入院基本料コード取得
               INITIALIZE    ORCSKANGOCHG-AREA
               MOVE   SYOKIKSN-IN-SANTEISTYMD(1:6)
                                   TO    LNK-KANGOCHG-SANTEIYM
               MOVE   SYS-5001-BTU-NUM
                                   TO    LNK-KANGOCHG-BTU-NUM
               MOVE   SYOKIKSN-IN-KHNSRYCD
                                   TO    LNK-KANGOCHG-SRYCD
               CALL   "ORCSKANGOCHG"    USING   ORCSKANGOCHG-AREA
                                                SPA-AREA
               IF   LNK-KANGOCHG-RC     =       ZERO
                    MOVE LNK-KANGOCHG-CHGSRYCD
                                   TO    SYOKIKSN-IN-KHNSRYCD
               END-IF
           END-IF
      *
           .
       4801-YAKINGEN-CHG-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料一般コードから老人コードへの置き換え処理
      *****************************************************************
       4801-RJNCD-CHG-SEC   SECTION.
      *
      *    老人対象患者チェック
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                   TO  WRK-AGECHK-SRYYMD
           PERFORM 4801-RJN-TAISYO-CHK-SEC
           IF    ( FLG-RJN         =   1 )    AND
                 ( COMB-HKNNUM     NOT =   "971" )  AND
                 ( COMB-HKNNUM     NOT =   "973" )  AND
                 ( COMB-HKNNUM     NOT =   "975" )
      *
      *        平成１８年４月改定対応
      *        老人コード置き換えは療養病棟、療養病床のみ
               IF   SYOKIKSN-IN-SANTEISTYMD(1:6)  >=  "200604"
                   IF   SYOKIKSN-IN-KHNSRYCD  =  "190080610" OR
                                                 "190080710" OR
                                                 "190081310" OR
                                                 "190081410" OR
                                                 "190098210" OR
                                                 "190098310"
                       INITIALIZE              SRYCDCHG-REC
                       MOVE    SYOKIKSN-IN-KHNSRYCD
                                           TO   CHG-IPNSRYCD
                       MOVE    SRYCDCHG-REC    TO   MCPDATA-REC
                       MOVE    "tbl_srycdchg"  TO MCP-TABLE
                       MOVE    "key2"          TO MCP-PATHNAME
                       PERFORM 910-DBSELECT-SEC 
                       IF    ( MCP-RC      =   ZERO )
                           MOVE    MCPDATA-REC
                                               TO   SRYCDCHG-REC
                           IF   CHG-RJNSRYCD   NOT =   SPACE
                                MOVE   CHG-RJNSRYCD
                                               TO  SYOKIKSN-IN-KHNSRYCD
                           END-IF
                       END-IF
      *
                       MOVE    "tbl_srycdchg"  TO MCP-TABLE
                       MOVE    "key2"          TO MCP-PATHNAME
                       PERFORM 910-DBCLOSECURSOR-SEC
                   END-IF
               ELSE
                   INITIALIZE              SRYCDCHG-REC
                   MOVE    SYOKIKSN-IN-KHNSRYCD
                                           TO   CHG-IPNSRYCD
                   MOVE    SRYCDCHG-REC    TO   MCPDATA-REC
                   MOVE    "tbl_srycdchg"  TO MCP-TABLE
                   MOVE    "key2"          TO MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC 
                   IF    ( MCP-RC      =   ZERO )
                       MOVE    MCPDATA-REC
                                           TO   SRYCDCHG-REC
                       IF   CHG-RJNSRYCD   NOT =   SPACE
                            MOVE   CHG-RJNSRYCD
                                           TO  SYOKIKSN-IN-KHNSRYCD
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_srycdchg"  TO MCP-TABLE
                   MOVE    "key2"          TO MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *
           .
       4801-RJNCD-CHG-EXT.
           EXIT.
      *****************************************************************
      *    老人対象患者チェック
      *****************************************************************
       4801-RJN-TAISYO-CHK-SEC         SECTION.
      *
      *    患者公費情報から老人保健の登録チェック
           MOVE    WRK-AGECHK-SRYYMD TO  WRK-RJNYMD
           PERFORM 410111-ROUJIN-CHK-SEC
      *
           MOVE    ZERO                TO  FLG-RJN
           MOVE    ZERO                TO  FLG-LIFE-RJN

      *    入院患者の老人保険判定
           MOVE    ZERO                TO   FLG-HKNCOMBI
           INITIALIZE              HKNCOMBI-REC
      *
           MOVE    SPA-HOSPNUM          TO   COMB-HOSPNUM
           MOVE    SPA-NAI-I01-PTID
                                       TO   COMB-PTID
           IF      SPA-GMN-I01-SYORIKBN-CD =   "07"  OR  "09"
               MOVE    WRK-TAIHI-HKN   TO   COMB-HKNCOMBINUM
           ELSE
               MOVE    SPA-GMN-I01-HKNCOMBI-CD
                                       TO   COMB-HKNCOMBINUM
           END-IF
           MOVE    HKNCOMBI-REC        TO   MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC 
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC
                                       TO   HKNCOMBI-REC
           ELSE
               MOVE    1               TO   FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    入院患者の年齢計算
           INITIALIZE ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM          TO  AGECHK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID
                                       TO  AGECHK-PTID
           MOVE    WRK-AGECHK-SRYYMD
                                       TO  AGECHK-SRYYMD
           MOVE    SPA-NAI-I01-BIRTHDAY
                                       TO  AGECHK-BIRTHDAY
           MOVE    SPA-GMN-I01-HKNCOMBI-CD
                                       TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
           IF    ( AGECHK-RC       =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSAGECHKAREA
           END-IF
      *
      *    老人対象患者判定
           IF    ( FLG-ROUJIN       =   1 )      OR
                 ( AGECHK-NENREI   >=    75)         OR
                 ((AGECHK-NENREI   >=    70)     AND
                  (PTINF-BIRTHDAY  <=   "19320930")) OR
                  (AGECHK-ROUJIN    =    1)
               MOVE    1            TO  FLG-RJN
           END-IF
      *
           IF    ( SPA-NAI-I01-RSI-HKNKBN  =   SPACE )
            OR   ( AGECHK-HKNKBN           =   4 OR 5 OR 7 )
               EVALUATE    TRUE
               WHEN  ( WRK-AGECHK-SRYYMD   >=  CONST-H200401 )
      *
                   IF    ( AGECHK-NENREI   >=   65 )
                    OR   ( FLG-ROUJIN       =   1 )
                       MOVE    1        TO  FLG-LIFE-RJN
                   END-IF
      *
               WHEN  ( WRK-AGECHK-SRYYMD   >=  CONST-H181001 )
      *
                   MOVE    FLG-RJN      TO  FLG-LIFE-RJN
      *
      *            老人対象患者判定
                   IF    ( FLG-ROUJIN           =   1  ) 
                    OR   ( AGECHK-NENREI       >=   70 )
                       MOVE    1        TO  FLG-LIFE-RJN
                   END-IF
               END-EVALUATE
      *
               IF    ( FLG-LIFE-RJN        =   1 )
                   IF    ( AGECHK-HKNKBN   =   4 OR 5 )
                       MOVE    2           TO  FLG-LIFE-RJN
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       4801-RJN-TAISYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理テーブル更新処理
      *****************************************************************
       4801-BUNBEN-UPD-SEC          SECTION.
      *
           MOVE    NRRK-RRKNUM     TO  WRK-BUNBENRRKNUM
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
           IF    ( FLG-BUNBEN      =   ZERO )
            AND  ( SPA-GMN-I01-BUNBEN-CD
                                   =   "1" OR "2" )
               MOVE    "000000"    TO  BUNBEN-SKYYM
               INITIALIZE              BUNBEN-TEISYUTUBIKBN
                                       BUNBEN-SKYKBN
                                       BUNBEN-NYUIN-DAY
                                       BUNBEN-NYU-MONEY
                                       BUNBEN-RMSGK-MONEY
                                       BUNBEN-KAIJO-MONEY
                                       BUNBEN-BUNBEN-MONEY
                                       BUNBEN-SHIN-MONEY
                                       BUNBEN-KNS-MONEY
                                       BUNBEN-SYC-MONEY
                                       BUNBEN-SNK-MONEY
                                       BUNBEN-ETC-MONEY
                                       BUNBEN-ICBFTN-MONEY
                                       BUNBEN-NINPUFTN-MONEY
                                       BUNBEN-DAIRI-MONEY
               IF    ( BUNBEN-UPDKBN   NOT =   ZERO )
                   MOVE    "3"     TO  BUNBEN-UPDKBN
               END-IF
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    NRRK-HOSPNUM
                                   TO  BUNBEN-HOSPNUM
               MOVE    NRRK-PTID   TO  BUNBEN-PTID
               MOVE    "1"         TO  BUNBEN-NYUGAIKBN
               MOVE    "0"         TO  BUNBEN-UPDKBN
               MOVE    SMCNDATE-YMD TO BUNBEN-CREYMD
               MOVE    NRRK-RRKNUM TO  BUNBEN-RRKNUM
               MOVE    "000000"    TO  BUNBEN-SKYYM
           END-IF
      *
           MOVE    SPA-GMN-I01-BUNBEN-CD
                                   TO  BUNBEN-BUNBENKBN
           MOVE    SPA-GMN-I01-RIYOU-CD
                                   TO  BUNBEN-RIYOUKBN
           MOVE    SMCNDATE-YMD    TO  BUNBEN-UPYMD
           MOVE    SMCNDATE-HMS    TO  BUNBEN-UPHMS
      *
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "del3"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "4034"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    NRRK-REC       TO   WKPTNYUIN-REC
               CALL    "ORCGI0SUB03"  USING
                                      WKPTNYUIN-REC
                                      SPA-AREA
               IF    ( WKPTNYUIN-RRKEDANUM  >   ZERO )
                AND  ( SPA-GMN-I01-BUNBEN-CD   =   "0" OR "1" OR "2" )
      *
                   MOVE    BUNBEN-REC      TO  MCPDATA-REC
                   MOVE    "tbl_bunben"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC          =   ZERO )
                       CONTINUE
                   ELSE
                       MOVE    "4035"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       4801-BUNBEN-UPD-EXT.
           EXIT.
      *****************************************************************
      *    患者情報更新処理
      *****************************************************************
       4801-PTINF-UPD-SEC          SECTION.
      *
           INITIALIZE                      LASTRRK-REC
      *
           MOVE    ZERO                TO  FLG-ZI-SAISIN
      *    入院歴より最新の歴情報を取得
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           PERFORM UNTIL ( MCP-RC          NOT =   ZERO )
                   OR    ( FLG-ZI-SAISIN   NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
               IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-ZI-SAISIN
                   MOVE    PTNYUINRRK-REC
                                       TO  LASTRRK-REC
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    患者マスタ検索
           MOVE    ZERO                TO  FLG-PTINF
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPNUM          TO  PTINF-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     ( MCP-RC             =  ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-PTINF           NOT =   ZERO )
               MOVE    "4026"          TO  SPA-ERRCD
               GO  TO  4801-PTINF-UPD-EXT
           END-IF
      *
      *    患者情報の入院歴番号を更新する
           IF    ( FLG-ZI-SAISIN       =   1 )
               MOVE   LASTRRK-RRKNUM   TO  PTINF-RRKNUM
               MOVE   LASTRRK-RRKEDANUM
                                       TO  PTINF-RRKEDANUM
           ELSE
               MOVE    ZERO            TO  PTINF-RRKNUM
                                           PTINF-RRKEDANUM
           END-IF
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
      *
      *    患者情報更新
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF     ( MCP-RC         NOT =  ZERO )
               MOVE    "4027"          TO  SPA-ERRCD
           END-IF
      *
           .
       4801-PTINF-UPD-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為照会テーブル作成処理
      *****************************************************************
       4801-ORCSS010-SEC               SECTION.
      *
           MOVE    NRRK-TENNYUYMD  TO  WRK-STYMD
           MOVE    WRK-LASTSRYYM   TO  WRK-EDYMD
      *
           PERFORM 900-NYUINACCT-KEY19-SEL-SEC
           IF    ( WRK-EDYMD       =   SPACE )
            OR   ( NACCT-SRYYM >=  WRK-EDYMD (1:6) )
               MOVE    NACCT-SRYYM TO  WRK-EDYMD
           END-IF
      *
           IF    ( WRK-EDYMD       =   SPACE )
               IF    ( SPA-GMN-I01-SYORIKBN-CD = "05" OR "06" )
      *
                   INITIALIZE                  NSRYSRH-REC
                   MOVE    SPA-HOSPNUM     TO  NSRYSRH-HOSPNUM
                   MOVE    SPA-NAI-I01-PTID
                                           TO  NSRYSRH-PTID
                   MOVE    SPA-NAI-I01-NYUINYMD (1:6)
                                           TO  NSRYSRH-SRYYM
                   MOVE    NSRYSRH-REC     TO  MCPDATA-REC
                   MOVE    "tbl_nsrysrh"   TO  MCP-TABLE
                   MOVE    "del3"          TO  MCP-PATHNAME
                   PERFORM 910-DBDELETE-SEC
      *
                   INITIALIZE                  NRRKSRH-REC
                   MOVE    SPA-HOSPNUM     TO  NRRKSRH-HOSPNUM
                   MOVE    SPA-NAI-I01-PTID
                                           TO  NRRKSRH-PTID
                   MOVE    SPA-NAI-I01-NYUINYMD
                                           TO  NRRKSRH-SRYYMD
                   MOVE    "31"            TO  NRRKSRH-SRYYMD(7:2)
                   MOVE    NRRKSRH-REC     TO  MCPDATA-REC
                   MOVE    "tbl_nrrksrh"   TO  MCP-TABLE
                   MOVE    "del3"          TO  MCP-PATHNAME
                   PERFORM 910-DBDELETE-SEC
                   MOVE    WRK-STYMD       TO  WRK-EDYMD
      *
               ELSE
                   MOVE    "4032"          TO  SPA-ERRCD
                   MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                   MOVE    "abort"         TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
               END-IF
           END-IF
      *
           PERFORM UNTIL ( WRK-STYMD (1:6) >   WRK-EDYMD (1:6))
                    OR   ( SPA-ERRCD  NOT  =   SPACE )
      *
               PERFORM 48011-NYUINACCT-KIGEN-CHK-SEC
               IF    ( FLG-PASS            =   ZERO )
      *
                   INITIALIZE                  SS010-AREA
                   MOVE    SPA-NAI-I01-PTID    TO  SS010-PTID
                   MOVE    WRK-STYMD (1:6)     TO  SS010-SRYYM
                   CALL    "ORCSS010"      USING   SS010-AREA
                                                   SPA-AREA
                   IF    ( SS010-RC        NOT =   ZERO )
                       MOVE    "4033"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               MOVE    WRK-STYMD       TO  WRK-YMD
               MOVE    "2"             TO  WRK-ZOGENPTN
               MOVE     1              TO  WRK-ZOGEN
               PERFORM 5002-CALENDAR-SEC
               MOVE    WRK-KEISANYMD   TO  WRK-STYMD
      *
           END-PERFORM
      *
           .
       4801-ORCSS010-EXT.
           EXIT.
      *****************************************************************
      *    入院会計診療行為有効期間チェック処理
      *****************************************************************
       48011-NYUINACCT-KIGEN-CHK-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-PASS
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SPA-NAI-I01-PTID    TO  NACCT-PTID
           MOVE    WRK-STYMD (1:6)     TO  NACCT-SRYYM
           PERFORM 900-NYUINACCT-KEY7-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               PERFORM 900-NYUINACT-KEY9-SEL-SEC
      *
               PERFORM VARYING IDX-NSRY    FROM    1   BY  1
                       UNTIL ( IDX-NSRY        >   5 )
                        OR   ( NSRY-SRYCD (IDX-NSRY)
                                               =   SPACE )
                        OR   ( FLG-PASS    NOT =   ZERO )
      *
                   INITIALIZE              TNS-TENSU-REC
                   MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
                   MOVE    NSRY-SRYCD (IDX-NSRY)
                                           TO  TNS-SRYCD
                   MOVE    NSRY-SRYYM      TO  TNS-YUKOSTYMD (1:6)
                   MOVE    "01"            TO  TNS-YUKOSTYMD (7:2)
                   MOVE    TNS-YUKOSTYMD   TO  TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
                   MOVE    "tbl_tensu"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC          =   ZERO )
                       CONTINUE
                   ELSE
                       MOVE    1           TO  FLG-PASS
                   END-IF
      *
                   MOVE    "tbl_tensu"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-PERFORM
      *
               PERFORM 900-NYUINACCT-KEY7-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC 
      *
           .
       48011-NYUINACCT-KIGEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療会計テーブルチェック処理
      *****************************************************************
       490-SRYACCT-CHK-SEC             SECTION.
      *
           MOVE    SPA-NAI-I01-TAIINYMD
                                       TO  WRK-TAIINYMD
      *
           MOVE    ZERO                TO  FLG-SRYACCT
      *
           INITIALIZE                      SRYACCT-REC
      *
           MOVE    SPA-HOSPNUM          TO  ACCT-HOSPNUM
           MOVE    "1"                 TO  ACCT-NYUGAIKBN
           MOVE    SPA-NAI-I01-PTID    TO  ACCT-PTID
           MOVE    WRK-TAIINYM         TO  ACCT-SRYYM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC 
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL ( FLG-SRYACCT     NOT =   ZERO )
                   OR    ( SPA-I0IDCD      NOT =   SPACE )
      *
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
      *
               IF    ( ACCT-SRYCDTOTAL =   120001110
                  OR                       120000410 )
                   CONTINUE
               ELSE
                   IF    ( WRK-TAIINYM     =   ACCT-SRYYM )
                       COMPUTE IDX14       =   WRK-TAIINDD +   1
                   ELSE
                       COMPUTE IDX14       =   1
                   END-IF
      *
                   PERFORM UNTIL ( IDX14               >   31 )
                           OR    ( SPA-I0IDCD      NOT =   SPACE )
      *
                       IF    ( ACCT-DAY (1 IDX14)      >   ZERO )
                           MOVE    "3004"          TO  SPA-I0IDCD
                           IF   SPA-GMN-I01-SYORIKBN-CD   =  "11"
                               MOVE  "3011"          TO  SPA-I0IDCD
                           END-IF
                       END-IF
      *
                       COMPUTE IDX14       =   IDX14   +   1
      *
                   END-PERFORM
               END-IF
      *
               IF    ( SPA-I0IDCD      =   SPACE )
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key15"             TO  MCP-PATHNAME
                   PERFORM 910-DBFETCH-SEC 
                   IF    ( MCP-RC              NOT =   ZERO )
                       MOVE    1               TO  FLG-SRYACCT
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC 
      *
           .
       490-SRYACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担金計算処理
      *****************************************************************
       490-FTNKIN-CALC-SEC             SECTION.
      *
      *    通算日数を求める
           MOVE    WRK-FTN-NYUINYMD
                                   TO  WRK-STYMD
      *
      *    請求開始日（入院日または前回定期請求終了日の翌日）を設定
           MOVE    WRK-NOW-SKYSTYMD
                                   TO  WRK-EDYMD
           PERFORM 5002-KIKAN-CALC-SEC
           IF    ( FLG-ORCSDAY     NOT =   ZERO )
               MOVE    "2008"      TO  SPA-ERRCD
               GO  TO  490-FTNKIN-CALC-EXT
           END-IF
      *
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    SPA-NAI-I01-PTID    TO  SPA-PTID
           MOVE    SPA-NAI-I01-BIRTHDAY
                                       TO  SPA-BIRTHDAY
      *
           INITIALIZE                      I0SUB02-LNK
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "02" )
               MOVE    ZERO            TO  I0SUB02-SYORIFLG
           ELSE
               MOVE    1               TO  I0SUB02-SYORIFLG
           END-IF
      *
           IF    ( WRK-FTN-PROCKBN     =   "1" )
               MOVE    SPA-NAI-I01-PTRRKNUM
                                       TO  I0SUB02-RRKNUM
           ELSE
               MOVE    SPA-NAI-I01-TRRKNUM (SPA-GMN-I01-NYUREKILST-SEL)
                                       TO  I0SUB02-RRKNUM
           END-IF
           MOVE    WRK-NISSU1          TO  I0SUB02-NISSU
           MOVE    WRK-NOW-SKYSTYMD    TO  I0SUB02-SKYSTYMD
           MOVE    WRK-FTN-TAIINYMD    TO  I0SUB02-SKYEDYMD
      *
           CALL    "ORCGI0SUB02"       USING
                                       SPA-AREA
                                       I0SUB02-LNK
           EVALUATE    TRUE
           WHEN  ( I0SUB02-S02-RC  NOT =   ZERO )
      *
               INITIALIZE                  S02MSG-AREA
               MOVE    I0SUB02-S02-RC  TO  S02MSG-S02-RC
               CALL    "ORCS02MSG"         USING
                                           S02MSG-AREA
                                           SPA-AREA
               IF    ( S02MSG-MSG (1)  =   SPACE )
                   MOVE    "2008"      TO  SPA-ERRCD
               ELSE
                   COMPUTE WRK-ERRCD-NUM = 3000 + I0SUB02-S02-RC
                   MOVE    WRK-ERRCD   TO  SPA-ERRCD
               END-IF
      *
           WHEN  ( I0SUB02-RC     NOT =   ZERO )
                   MOVE    "2008"  TO  SPA-ERRCD
           WHEN    OTHER
      *
               MOVE    I0SUB02-SYUNOU-CNT  TO  SPA-I0-SYUNOU-CNT
               PERFORM VARYING IDX-SYU FROM    1   BY  1
                       UNTIL ( IDX-SYU >   I0SUB02-SYUNOU-CNT )
      *
                   MOVE    I0SUB02-SYUNOU-TTLDAT-FLG(IDX-SYU)
                                   TO  SPA-I0-SYUNOU-TTLDAT-FLG(IDX-SYU)
                   MOVE    I0SUB02-SYUNOU-UPDFLG(IDX-SYU)
                                   TO  SPA-I0-SYUNOU-UPDFLG(IDX-SYU)
      *
                   MOVE    I0SUB02-SYUNOU-YM-S (IDX-SYU)
                                   TO  SPA-I0-SYUNOU-YM-S (IDX-SYU)
                   MOVE    I0SUB02-SYUNOU-YM-W (IDX-SYU)
                                   TO  SPA-I0-SYUNOU-YM-W (IDX-SYU)
                   MOVE    I0SUB02-SYUNOU-HKNCOMBI (IDX-SYU)
                                   TO  SPA-I0-SYUNOU-HKNCOMBI
                                                          (IDX-SYU)
                   MOVE    I0SUB02-SYUNOU-HKNCOMBI-SU (IDX-SYU)
                                   TO  SPA-I0-SYUNOU-HKNCOMBI-SU
                                                          (IDX-SYU)
                   MOVE    I0SUB02-SYUNOU-TUKI-SU (IDX-SYU)
                                   TO  SPA-I0-SYUNOU-TUKI-SU
                                                          (IDX-SYU)
                   MOVE    I0SUB02-SYUNOU-DAT  (IDX-SYU)
                                   TO  SPA-I0-SYUNOU-DAT  (IDX-SYU)
                   MOVE    I0SUB02-SYUDAY-DAT  (IDX-SYU)
                                   TO  SPA-I0-SYUDAY-DAT (IDX-SYU)
                   MOVE    I0SUB02-UPD-SKYMONEY (IDX-SYU)
                                   TO  SPA-I0-UPD-SKYMONEY(IDX-SYU)
                   MOVE    I0SUB02-UPD-TAX-SAI  (IDX-SYU)
                                   TO  SPA-I0-UPD-TAX-SAI (IDX-SYU)
               END-PERFORM
           END-EVALUATE
      *
           .
       490-FTNKIN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    請求確認画面遷移処理
      *****************************************************************
       490-SYUNOU-SUM-SEC              SECTION.
      *
      *
       490-SYUNOU-SUM-EXT.
           EXIT.
      *****************************************************************
      *    退院処理
      *****************************************************************
       490-TAIIN-SYORI-SEC             SECTION.
      *
      *    負担金計算処理
           MOVE    SPA-NAI-I01-NYUINYMD    TO  WRK-FTN-NYUINYMD
           MOVE    SPA-NAI-I01-TAIINYMD    TO  WRK-FTN-TAIINYMD
           MOVE    SPA-NAI-I01-DJKBN       TO  WRK-FTN-DJKBN
           MOVE    "1"                     TO  WRK-FTN-PROCKBN
           PERFORM 490-FTNKIN-CALC-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  490-TAIIN-SYORI-EXT
           END-IF
      *
      *    請求確認画面遷移処理
           MOVE    "1"             TO  WRK-I01-I04-PROCKBN
           PERFORM 490-I04-SENI-SEC
           .
      *
       490-TAIIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    退院処理
      *****************************************************************
       490-TAIIN-SAIKEISAN-SYORI-SEC   SECTION.
      *
      *    請求開始日取得処理
           MOVE    SPA-NAI-I01-TNYUINYMD (IDX1)
                                           TO  WRK-FTN-NYUINYMD
           MOVE    SPA-NAI-I01-TRRKNUM (IDX1)
                                           TO  WRK-FTN-RRKNUM
           MOVE    SPA-NAI-I01-TDJKBN (IDX1)
                                           TO  WRK-FTN-DJKBN
           PERFORM 4901-SKYSTYMD-GET-SEC
      *
      *    負担金計算処理
           MOVE    SPA-NAI-I01-TNYUINYMD (IDX1)
                                           TO  WRK-FTN-NYUINYMD
           MOVE    SPA-NAI-I01-TTAIINYMD (IDX1)
                                           TO  WRK-FTN-TAIINYMD
           MOVE    "2"                     TO  WRK-FTN-PROCKBN
           PERFORM 490-FTNKIN-CALC-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  490-TAIIN-SAIKEISAN-SYORI-EXT
           END-IF
      *
      *    請求確認画面遷移処理
           MOVE    "2"             TO  WRK-I01-I04-PROCKBN
           PERFORM 490-I04-SENI-SEC
           .
      *
       490-TAIIN-SAIKEISAN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    請求確認画面遷移処理
      *****************************************************************
       490-I04-SENI-SEC                SECTION.
      *
           INITIALIZE  SPA-I01-I04-AREA
           MOVE    WRK-I01-I04-PROCKBN TO  SPA-I01-I04-PROCKBN
           MOVE    SPA-GMN-I01-SYORIKBN-CD
                                       TO  SPA-I01-I04-SYORIKBN
           IF    ( WRK-I01-I04-PROCKBN =   "1" )
               MOVE    SPA-NAI-I01-PTRRKNUM
                                           TO  SPA-I01-I04-RRKNUM
               MOVE    SPA-NAI-I01-PTRRKEDANUM
                                           TO  SPA-I01-I04-RRKEDANUM
               MOVE    SPA-NAI-I01-MAEIDOYMD
                                           TO  SPA-I01-I04-KJNYMD
               MOVE    SPA-GMN-I01-HKNCOMBI-CD
                                           TO  SPA-I01-I04-HKNCOMBI
               MOVE    SPA-GMN-I01-NYUINKA-CD
                                           TO  SPA-I01-I04-NYUINKA
               MOVE    SPA-NAI-I01-NYUINYMD
                                           TO  SPA-I01-I04-NYUINYMD
               MOVE    SPA-NAI-I01-TAIINYMD
                                           TO  SPA-I01-I04-TAIINYMD
               MOVE    SPA-GMN-I01-TAIINCD
                                           TO  SPA-I01-I04-TAIINCD
               MOVE    WRK-NOW-SKYSTYMD    TO  SPA-I01-I04-SKYSTYMD
               MOVE    SPA-NAI-I01-TAIINYMD
                                           TO  SPA-I01-I04-SKYEDYMD
               MOVE    SPA-NAI-I01-TORIKESI-FLG
                                           TO  SPA-I01-I04-TORIKESI-FLG
               PERFORM 41011-BUNBEN-NYUIN-CHK-SEC
               IF    ( FLG-BUNBENCHK           =   1 )
                 AND ( SPA-GMN-I01-BUNBEN-CD   =   "1" OR "2" )
                 AND ( SPA-I01-I04-TAIINYMD   >=   CONST-H211001 )
                   MOVE    1               TO  SPA-I01-I04-BUNBENFTN-FLG
               END-IF
           ELSE
               PERFORM 4901-PTNYUINRRK-SEL-SEC
               MOVE    PTNYUINRRK-RRKNUM   TO  SPA-I01-I04-RRKNUM
               MOVE    PTNYUINRRK-RRKEDANUM
                                           TO  SPA-I01-I04-RRKEDANUM
               MOVE    PTNYUINRRK-TENNYUYMD
                                           TO  SPA-I01-I04-KJNYMD
               MOVE    PTNYUINRRK-HKNCOMBINUM
                                           TO  SPA-I01-I04-HKNCOMBI
               MOVE    PTNYUINRRK-NYUINKA  TO  SPA-I01-I04-NYUINKA
               MOVE    PTNYUINRRK-NYUINYMD TO  SPA-I01-I04-NYUINYMD
               MOVE    PTNYUINRRK-TAIINYMD TO  SPA-I01-I04-TAIINYMD
               MOVE    PTNYUINRRK-TAIINCD  TO  SPA-I01-I04-TAIINCD
      *
               MOVE    WRK-NOW-SKYSTYMD    TO  SPA-I01-I04-SKYSTYMD
      *
               MOVE    PTNYUINRRK-TAIINYMD TO  SPA-I01-I04-SKYEDYMD
      *
               IF    ( SPA-NAI-I01-TBUNBEN (SPA-GMN-I01-NYUREKILST-SEL)
                                           =   1 )
                   MOVE    1               TO  SPA-I01-I04-BUNBENFTN-FLG
               END-IF
      *
           END-IF
      *
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "I04"               TO  SPA-SAKIPG
      *
           MOVE   WRK-MCP-PUTTYPE      TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       490-I04-SENI-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理（退院再計算処理）
      *****************************************************************
       4901-PTNYUINRRK-SEL-SEC         SECTION.
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-I01-TRRKNUM (SPA-GMN-I01-NYUREKILST-SEL)
                                       TO  PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
      *
       4901-PTNYUINRRK-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求開始日取得処理（退院再計算処理）
      *****************************************************************
       4901-SKYSTYMD-GET-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-NOW-SKYSTYMD
      *
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTTEIKIRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTTEIKIRRK-PTID
           MOVE    WRK-FTN-RRKNUM      TO  PTTEIKIRRK-RRKNUM
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
               MOVE    PTTEIKIRRK-SKYEDYMD
                                       TO  WRK-YMD
               MOVE    "1"             TO  WRK-ZOGENPTN
               MOVE    1               TO  WRK-ZOGEN
               PERFORM 5002-CALENDAR-SEC
               MOVE    WRK-KEISANYMD   TO  WRK-NOW-SKYSTYMD
           END-IF
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( WRK-NOW-SKYSTYMD    =   SPACE )
               MOVE    WRK-FTN-NYUINYMD
                                       TO  WRK-NOW-SKYSTYMD
               IF    ( WRK-NOW-SKYSTYMD  <   CONST-H180401 )
                   IF    ( WRK-FTN-DJKBN   =   "1" )
                       MOVE    WRK-FTN-NYUINYMD
                                       TO  WRK-YMD
                       MOVE    "1"     TO  WRK-ZOGENPTN
                       MOVE    1       TO  WRK-ZOGEN
                       PERFORM 5002-CALENDAR-SEC
                       MOVE    WRK-KEISANYMD
                                       TO  WRK-NOW-SKYSTYMD
                   END-IF
               END-IF
           END-IF
      *
           .
       4901-SKYSTYMD-GET-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-I0IDCD      NOT =   SPACE )
               PERFORM 520-I0IDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-I0IDCD2 NOT =   SPACE )
               PERFORM 520-I0ID2SET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I01    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE                   TO  I01
           INITIALIZE                          I01
      *
      *    患者番号
           MOVE    SPA-GMN-I01-PTNUM       TO  I01-PTNUM
      *    患者氏名
           MOVE    SPA-GMN-I01-NAME        TO  I01-NAME
           IF    ( SPA-NAI-I01-LOCK        =   1 )
               MOVE    "red"               TO  I01-NAME-STYLE
           ELSE
               MOVE    "black"             TO  I01-NAME-STYLE
           END-IF
      *    患者性別
           MOVE    SPA-GMN-I01-SEX         TO  I01-SEX
      *    患者誕生日
           MOVE    SPA-GMN-I01-BIRTHDAY    TO  I01-BIRTHDAY
      *    患者年齢
           MOVE    SPA-GMN-I01-AGE         TO  I01-AGE
      *    処理区分
           MOVE    SPA-GMN-I01-SYORIKBN    TO  I01-SYORIKBN
           MOVE    SPA-GMN-I01-SYORIKBN-CNT
                                           TO  I01-SYORIKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-SYORIKBN-CNT )
               MOVE    SPA-GMN-I01-SYORIKBN-ITM (IDX1)
                                           TO  I01-SYORIKBN-ITM (IDX1)
           END-PERFORM
      *
      *    異動日
           MOVE    SPA-GMN-I01-IDOYMD      TO  I01-IDOYMD
      *
      *    前回異動日
           MOVE    SPA-GMN-I01-MAEIDOYMD   TO  I01-MAEIDOYMD
      *
      *    注意情報
           MOVE    "black"                 TO  I01-LBLINFO-STYLE
           IF    ( SPA-GMN-I01-LBLINFO     NOT =   SPACE )
               MOVE    "red"               TO  I01-LBLINFO-STYLE
               MOVE    SPA-GMN-I01-LBLINFO TO  I01-LBLINFO
           END-IF
      *
      *    病室番号
           MOVE    SPA-GMN-I01-BRMNUM      TO  I01-BRMNUM
      *    病棟名
           MOVE    SPA-GMN-I01-BTUNAME     TO  I01-BTUNAME
           MOVE    SPA-GMN-I01-BTUNAME-CNT TO  I01-BTUNAME-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-BTUNAME-CNT )
               MOVE    SPA-GMN-I01-BTUNAME-ITM (IDX1)
                                           TO  I01-BTUNAME-ITM (IDX1)
           END-PERFORM
      *
      *    室料差額
           MOVE    SPA-GMN-I01-SAGAKU      TO  I01-SAGAKU
           MOVE    SPA-GMN-I01-SAGAKU-CNT  TO  I01-SAGAKU-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-SAGAKU-CNT )
               MOVE    SPA-GMN-I01-SAGAKU-ITM (IDX1)
                                           TO  I01-SAGAKU-ITM (IDX1)
           END-PERFORM
      *
      *    食事区分
           MOVE    SPA-GMN-I01-SKJKBN      TO  I01-SKJKBN
           MOVE    SPA-GMN-I01-SKJKBN-CNT  TO  I01-SKJKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-SKJKBN-CNT )
               MOVE    SPA-GMN-I01-SKJKBN-ITM (IDX1)
                                           TO  I01-SKJKBN-ITM (IDX1)
           END-PERFORM
      *
      *    食事区分朝昼夕
           IF    ( SPA-GMN-I01-SKJKBN-CD   =   "B" OR "C" )
               MOVE    "朝"                    TO  I01-LBL-TSKJKBN (1)
               MOVE    "昼"                    TO  I01-LBL-TSKJKBN (2)
               MOVE    "夕"                    TO  I01-LBL-TSKJKBN (3)
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   3 )
                   MOVE    SPA-GMN-I01-TSKJKBN (IDX1)
                                       TO  I01-TSKJKBN (IDX1)
                   MOVE    SPA-GMN-I01-TSKJKBN-CNT
                                       TO  I01-TSKJKBN-CNT (IDX1)
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL ( IDX2    >   SPA-GMN-I01-TSKJKBN-CNT )
                       MOVE    SPA-GMN-I01-TSKJKBN-ITM (IDX2)
                                       TO  I01-TSKJKBN-ITM (IDX1 IDX2)
                   END-PERFORM
      *
               END-PERFORM
           END-IF
      *
      *    食事時間区分
           MOVE    SPA-GMN-I01-TIMEKBN      TO  I01-TIMEKBN
           MOVE    SPA-GMN-I01-TIMEKBN-CNT  TO  I01-TIMEKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-TIMEKBN-CNT )
               MOVE    SPA-GMN-I01-TIMEKBN-ITM (IDX1)
                                           TO  I01-TIMEKBN-ITM (IDX1)
           END-PERFORM
      *
      *    選定入院区分
           MOVE    SPA-GMN-I01-SENTEIKBN   TO  I01-SENTEIKBN
           MOVE    SPA-GMN-I01-SENTEIKBN-CNT
                                           TO  I01-SENTEIKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-SENTEIKBN-CNT )
               MOVE    SPA-GMN-I01-SENTEIKBN-ITM (IDX1)
                                           TO  I01-SENTEIKBN-ITM (IDX1)
           END-PERFORM
      *
      *    入院日
           MOVE    SPA-GMN-I01-NYUINYMD    TO  I01-NYUINYMD
      *    入院科
           MOVE    SPA-GMN-I01-NYUINKA     TO  I01-NYUINKA
           MOVE    SPA-GMN-I01-NYUINKA-CNT TO  I01-NYUINKA-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-NYUINKA-CNT )
               MOVE    SPA-GMN-I01-NYUINKA-ITM (IDX1)
                                           TO  I01-NYUINKA-ITM (IDX1)
           END-PERFORM
      *
      *    初回
           MOVE    SPA-GMN-I01-SHOKAI      TO  I01-SHOKAI
           MOVE    SPA-GMN-I01-SHOKAI-CNT  TO  I01-SHOKAI-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-SHOKAI-CNT )
               MOVE    SPA-GMN-I01-SHOKAI-ITM (IDX1)
                                           TO  I01-SHOKAI-ITM (IDX1)
           END-PERFORM
      *
      *    初回番号
           MOVE    SPA-GMN-I01-SHONUM      TO  I01-SHONUM
      *
      *    初回入院日
           MOVE    SPA-GMN-I01-LBL-SHONYUINYMD
                                           TO  I01-LBL-SHONYUINYMD
      *
      *    介護異動
           MOVE    "介護からの異動"    TO  I01-LBL-KAIGO
           MOVE    SPA-GMN-I01-KAIGO       TO  I01-KAIGO
           MOVE    SPA-GMN-I01-KAIGO-CNT   TO  I01-KAIGO-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-KAIGO-CNT )
               MOVE    SPA-GMN-I01-KAIGO-ITM (IDX1)
                                           TO  I01-KAIGO-ITM (IDX1)
           END-PERFORM
      *
      *    退院日
           MOVE    SPA-GMN-I01-TAIINYMD    TO  I01-TAIINYMD
      *
      *    退院事由
           MOVE    SPA-GMN-I01-TAIINCD     TO  I01-TAIINCD
           MOVE    SPA-GMN-I01-TAIINCD-CNT TO  I01-TAIINCD-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-TAIINCD-CNT )
               MOVE    SPA-GMN-I01-TAIINCD-ITM (IDX1)
                                           TO  I01-TAIINCD-ITM (IDX1)
           END-PERFORM
      *
      *    担当医
           PERFORM VARYING IDX1   FROM     1   BY  1
               UNTIL     ( IDX1   >    3   )
               MOVE    SPA-GMN-I01-DR (IDX1)
                                           TO  I01-DR (IDX1)
               MOVE    SPA-GMN-I01-DR-CNT  TO  I01-DR-CNT (IDX1)
               PERFORM VARYING IDX2   FROM     1   BY  1
                   UNTIL     ( IDX2   >    SPA-GMN-I01-DR-CNT )
                   MOVE    SPA-GMN-I01-DR-ITM (IDX2)
                                   TO  I01-DR-ITM (IDX1 IDX2)
               END-PERFORM
           END-PERFORM
      *
      *    保険組合わせ
           MOVE    SPA-GMN-I01-HKNCOMBI    TO  I01-HKNCOMBI
           MOVE    SPA-GMN-I01-HKNCOMBI-CNT
                                           TO  I01-HKNCOMBI-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-HKNCOMBI-CNT   )
               MOVE    SPA-GMN-I01-HKNCOMBI-ITM (IDX1)
                                           TO  I01-HKNCOMBI-ITM (IDX1)
           END-PERFORM
      *
           IF    ( SPA-NAI-I01-SELNYUKHN   =   ZERO )
               MOVE    "特定入院料"        TO  I01-LBL-TOKNYUIN
           ELSE
               MOVE    "入院料選択"        TO  I01-LBL-TOKNYUIN
           END-IF
      *
      *    特定入院料
           MOVE    SPA-GMN-I01-TOKNYUIN    TO  I01-TOKNYUIN
           MOVE    SPA-GMN-I01-TOKNYUIN-CNT
                                           TO  I01-TOKNYUIN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-TOKNYUIN-CNT   )
               MOVE    SPA-GMN-I01-TOKNYUIN-ITM (IDX1)
                                           TO  I01-TOKNYUIN-ITM (IDX1)
           END-PERFORM
      *
      *    特定入院料非該当
           IF    ( SPA-NAI-I01-TOKHIGAITOCD    NOT =   SPACE )
               MOVE    SPA-GMN-I01-TOKHIGAITO    TO  I01-TOKHIGAITO
               MOVE    SPA-GMN-I01-TOKHIGAITO-CNT
                                           TO  I01-TOKHIGAITO-CNT
               PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >   SPA-GMN-I01-TOKHIGAITO-CNT )
                   MOVE    SPA-GMN-I01-TOKHIGAITO-ITM (IDX1)
                                           TO  I01-TOKHIGAITO-ITM (IDX1)
               END-PERFORM
           END-IF
      *
      *    算定入院料
           MOVE    SPA-GMN-I01-SNTNYUIN    TO  I01-SNTNYUIN
      *
      *    入院料算定
           MOVE    SPA-GMN-I01-NYUCALKBN   TO  I01-NYUCALKBN
           MOVE    SPA-GMN-I01-NYUCALKBN-CNT
                                           TO  I01-NYUCALKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-NYUCALKBN-CNT   )
               MOVE    SPA-GMN-I01-NYUCALKBN-ITM (IDX1)
                                           TO  I01-NYUCALKBN-ITM (IDX1)
           END-PERFORM
      *
      *    入院料加算
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-GMN-KSN-MAX )
               MOVE    SPA-GMN-I01-LBLKSN (IDX-KSN)
                                           TO  I01-LBLKSN (IDX-KSN)
               MOVE    SPA-GMN-I01-KSN    (IDX-KSN)
                                           TO  I01-KSN    (IDX-KSN)
               MOVE    SPA-GMN-I01-KSN-CNT(IDX-KSN)
                                           TO  I01-KSN-CNT(IDX-KSN)
               MOVE    SPA-GMN-I01-KSN-CNT(IDX-KSN)
                                           TO  WRK-CNT
               PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >   WRK-CNT   )
                   MOVE    SPA-GMN-I01-KSN-ITM (IDX-KSN IDX1)
                                           TO  I01-KSN-ITM(IDX-KSN IDX1)
               END-PERFORM
           END-PERFORM
      *
      *    前受け金
      *    MOVE    SPA-GMN-I01-MAEUKE      TO  I01-MAEUKE
      *
      *    定期請求
           MOVE    SPA-GMN-I01-SEIKYU      TO  I01-SEIKYU
           MOVE    SPA-GMN-I01-SEIKYU-CNT  TO  I01-SEIKYU-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-SEIKYU-CNT   )
               MOVE    SPA-GMN-I01-SEIKYU-ITM (IDX1)
                                           TO  I01-SEIKYU-ITM (IDX1)
           END-PERFORM
      *
      *    検索時患者表示
           MOVE    SPA-GMN-I01-DISPKBN     TO  I01-DISPKBN
           MOVE    SPA-GMN-I01-DISPKBN-CNT TO  I01-DISPKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-DISPKBN-CNT   )
               MOVE    SPA-GMN-I01-DISPKBN-ITM (IDX1)
                                           TO  I01-DISPKBN-ITM (IDX1)
           END-PERFORM
      *
      *    ユーザープログラム
           MOVE    SPA-GMN-I01-USERPG      TO  I01-USERPG
           MOVE    SPA-GMN-I01-USERPG-CNT  TO  I01-USERPG-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-USERPG-CNT   )
               MOVE    SPA-GMN-I01-USERPG-ITM (IDX1)
                                           TO  I01-USERPG-ITM (IDX1)
           END-PERFORM
      *
               MOVE    "分娩区分"               TO  I01-LBL-BUNBENKBN
           PERFORM 41011-BUNBEN-NYUIN-CHK-SEC
           IF    ( FLG-BUNBENCHK           =   1 )
      *
      *        分娩入院
               MOVE    SPA-GMN-I01-BUNBEN       TO  I01-BUNBENKBN
               MOVE    SPA-GMN-I01-BUNBEN-CNT   TO  I01-BUNBENKBN-CNT
               PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >   SPA-GMN-I01-BUNBEN-CNT )
                   MOVE    SPA-GMN-I01-BUNBEN-ITM (IDX1)
                                           TO  I01-BUNBENKBN-ITM (IDX1)
               END-PERFORM
           END-IF
      *
               MOVE    "直接支払制度"          TO  I01-LBL-RIYOUKBN
           IF    ( SPA-GMN-I01-BUNBEN-CD       =   "1" OR "2" )
      *
      *        直接支払制度利用区分
               MOVE    SPA-GMN-I01-RIYOU       TO  I01-RIYOUKBN
               MOVE    SPA-GMN-I01-RIYOU-CNT   TO  I01-RIYOUKBN-CNT
               PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >   SPA-GMN-I01-RIYOU-CNT )
                   MOVE    SPA-GMN-I01-RIYOU-ITM (IDX1)
                                           TO  I01-RIYOUKBN-ITM (IDX1)
               END-PERFORM
           END-IF
      *
      *    選択番号
           MOVE    SPA-GMN-I01-SELNUM      TO  I01-SELNUM
      *
      *    入院歴リスト
           MOVE    SPA-GMN-I01-NYUREKILST-CNT
                                           TO  I01-NYUREKILST-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL ( IDX1    >   SPA-GMN-I01-NYUREKILST-CNT )
               MOVE    SPA-GMN-I01-TSELNUM (IDX1)
                                       TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  I01-TSELNUM (IDX1)
               IF    ( SPA-GMN-I01-TSHOKAI (IDX1)  =   "1" )
                   MOVE    "◎"        TO  I01-TSHOKAI (IDX1)
               ELSE
                   MOVE    SPACE       TO  I01-TSHOKAI (IDX1)
               END-IF
               IF    ( SPA-GMN-I01-TKAIGO (IDX1)  =   "2" )
                   MOVE    "介"        TO  I01-TSHOKAI (IDX1)
               END-IF
               MOVE    SPA-GMN-I01-TSHONUM (IDX1)
                                       TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9
                                       TO  I01-TSHONUM (IDX1)
               MOVE    SPA-GMN-I01-TBTUNAME (IDX1)
                                       TO  I01-TBTUNAME (IDX1)
               MOVE    SPA-GMN-I01-TBRMNUM (IDX1)
                                       TO  I01-TBRMNUM (IDX1)
               MOVE    SPA-GMN-I01-TNYUINYMD (IDX1)
                                       TO  I01-TNYUINYMD (IDX1)
               MOVE    SPA-GMN-I01-TTAIINYMD (IDX1)
                                       TO  I01-TTAIINYMD (IDX1)
               MOVE    SPA-GMN-I01-TNYUINKA (IDX1)
                                       TO  I01-TNYUINKA (IDX1)
               MOVE    SPA-GMN-I01-TBYOMEI (IDX1)
                                       TO  I01-TBYOMEI (IDX1)
               MOVE    SPA-GMN-I01-TTENKI (IDX1)
                                       TO  I01-TTENKI (IDX1)
               MOVE    SPA-GMN-I01-TNISSU (IDX1)
                                       TO  I01-TNISSU (IDX1)
               MOVE    SPA-GMN-I01-TTUSAN (IDX1)
                                       TO  I01-TTUSAN (IDX1)
      *
               IF    ( SPA-GMN-I01-NYUREKILST-SEL  =   IDX1)
                   MOVE    "T"         TO  I01-NYUREKILST-SELECT (IDX1)
      *
                   IF    ( SPA-NAI-I01-TBUNBEN (IDX1)      =   1 )
                       MOVE    "出産一時金"
                                       TO  I01-B15-LABEL
                   END-IF
      *
               ELSE
                   MOVE    "F"         TO  I01-NYUREKILST-SELECT (IDX1)
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-I01-ORDERKBN    =   "1" )
               MOVE    "オーダー"      TO  I01-B24-LABEL
           END-IF
      *
           EVALUATE    FLG-NYUREKILST-ROW
           WHEN    1
               MOVE    ZERO            TO  I01-NYUREKILST-ROW
               MOVE    ZERO            TO  I01-NYUREKILST-ROWATTR
           WHEN    OTHER
               MOVE    SPA-GMN-I01-NYUREKILST-SEL
                                       TO  I01-NYUREKILST-ROW
               MOVE    2               TO  I01-NYUREKILST-ROWATTR
           END-EVALUATE
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  WRK-WIDGET-STATE
           PERFORM 5001-SET-WIDGET-STATE-SEC
      *
      *    処理区分応じて画面編集の内容を変更する
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
      *    入院登録
           WHEN    "01"
      *
               MOVE    WIDGET-INSENSITIVE
                                   TO  I01-IDOYMD-STT
                                       I01-TAIINYMD-STT
                                       I01-TAIINCD-STT
                                       I01-TAIINCD-ITM-STT
                                       I01-SHONUM-STT
                                       I01-KAIGO-STT
      *
               IF    ( SPA-NAI-I01-MAEIDOYMD   NOT =   SPACE )
                   MOVE    WIDGET-INSENSITIVE
                                   TO  I01-B07-STT
                                       I01-NYUINYMD-STT
               END-IF
      *
      *    退院登録
           WHEN    "02"
           WHEN    "11"
      *
      *
               MOVE    WIDGET-INSENSITIVE
                                   TO  I01-B07-STT
                                       I01-IDOYMD-STT
                                       I01-BRMNUM-STT
                                       I01-BTUNAME-STT
                                       I01-BTUNAME-ITM-STT
                                       I01-SAGAKU-STT
                                       I01-SAGAKU-ITM-STT
                                       I01-SKJKBN-STT
                                       I01-SKJKBN-ITM-STT
                                       I01-TSKJKBN-STT (1)
                                       I01-TSKJKBN-STT (2)
                                       I01-TSKJKBN-STT (3)
                                       I01-TSKJKBN-ITM-STT (1)
                                       I01-TSKJKBN-ITM-STT (2)
                                       I01-TSKJKBN-ITM-STT (3)
                                       I01-TIMEKBN-STT
                                       I01-TIMEKBN-ITM-STT
                                       I01-SENTEIKBN-STT
                                       I01-SENTEIKBN-ITM-STT
                                       I01-NYUINYMD-STT
                                       I01-NYUINKA-STT
                                       I01-NYUINKA-ITM-STT
                                       I01-SHOKAI-STT
                                       I01-SHOKAI-ITM-STT
                                       I01-SHONUM-STT
                                       I01-KAIGO-STT
                                       I01-KAIGO-ITM-STT
                                       I01-DR-STT (1)
                                       I01-DR-ITM-STT (1)
                                       I01-DR-STT (2)
                                       I01-DR-ITM-STT (2)
                                       I01-DR-STT (3)
                                       I01-DR-ITM-STT (3)
                                       I01-HKNCOMBI-STT
                                       I01-HKNCOMBI-ITM-STT
                                       I01-TOKNYUIN-STT
                                       I01-TOKNYUIN-ITM-STT
                                       I01-TOKHIGAITO-STT
                                       I01-TOKHIGAITO-ITM-STT
                                       I01-SNTNYUIN-STT
                                       I01-NYUCALKBN-STT
                                       I01-NYUCALKBN-ITM-STT
                                       I01-KSN-STT (1)
                                       I01-KSN-ITM-STT (1)
                                       I01-KSN-STT (2)
                                       I01-KSN-ITM-STT (2)
                                       I01-KSN-STT (3)
                                       I01-KSN-ITM-STT (3)
                                       I01-SEIKYU-STT
                                       I01-SEIKYU-ITM-STT
                                       I01-DISPKBN-STT
                                       I01-DISPKBN-ITM-STT
                                       I01-USERPG-STT
                                       I01-USERPG-ITM-STT
                                       I01-BUNBENKBN-STT
                                       I01-BUNBENKBN-ITM-STT
                                       I01-RIYOUKBN-STT
                                       I01-RIYOUKBN-ITM-STT
      *
      *    変更
           WHEN    "03"
      *
               MOVE    WIDGET-INSENSITIVE
                                   TO  I01-B07-STT
                                       I01-IDOYMD-STT
                                       I01-BRMNUM-STT
                                       I01-BTUNAME-STT
                                       I01-BTUNAME-ITM-STT
                                       I01-SAGAKU-STT
                                       I01-SAGAKU-ITM-STT
                                       I01-SKJKBN-STT
                                       I01-SKJKBN-ITM-STT
                                       I01-TSKJKBN-STT (1)
                                       I01-TSKJKBN-STT (2)
                                       I01-TSKJKBN-STT (3)
                                       I01-TSKJKBN-ITM-STT (1)
                                       I01-TSKJKBN-ITM-STT (2)
                                       I01-TSKJKBN-ITM-STT (3)
                                       I01-TIMEKBN-STT
                                       I01-TIMEKBN-ITM-STT
                                       I01-SENTEIKBN-STT
                                       I01-SENTEIKBN-ITM-STT
                                       I01-NYUINYMD-STT
                                       I01-NYUINKA-STT
                                       I01-NYUINKA-ITM-STT
                                       I01-SHOKAI-STT
                                       I01-SHOKAI-ITM-STT
                                       I01-SHONUM-STT
                                       I01-KAIGO-STT
                                       I01-KAIGO-ITM-STT
                                       I01-TAIINYMD-STT
                                       I01-TAIINCD-STT
                                       I01-TAIINCD-ITM-STT
                                       I01-HKNCOMBI-STT
                                       I01-HKNCOMBI-ITM-STT
                                       I01-TOKNYUIN-STT
                                       I01-TOKNYUIN-ITM-STT
                                       I01-TOKHIGAITO-STT
                                       I01-TOKHIGAITO-ITM-STT
                                       I01-SNTNYUIN-STT
                                       I01-NYUCALKBN-STT
                                       I01-NYUCALKBN-ITM-STT
                                       I01-KSN-STT (1)
                                       I01-KSN-ITM-STT (1)
                                       I01-KSN-STT (2)
                                       I01-KSN-ITM-STT (2)
                                       I01-KSN-STT (3)
                                       I01-KSN-ITM-STT (3)
      *    照会
           WHEN    "04"
               MOVE    WIDGET-INSENSITIVE  TO  WRK-WIDGET-STATE
               PERFORM 5001-SET-WIDGET-STATE-SEC
      *    転科　転棟　転室
           WHEN    "08"
      *
               MOVE    WIDGET-INSENSITIVE
                                   TO  I01-B07-STT
      **                                 I01-SKJKBN-STT
      **                                 I01-SKJKBN-ITM-STT
                                       I01-TIMEKBN-STT
                                       I01-TIMEKBN-ITM-STT
                                       I01-NYUINYMD-STT
                                       I01-SHOKAI-STT
                                       I01-SHOKAI-ITM-STT
                                       I01-SHONUM-STT
                                       I01-KAIGO-STT
                                       I01-KAIGO-ITM-STT
                                       I01-TAIINYMD-STT
                                       I01-TAIINCD-STT
                                       I01-TAIINCD-ITM-STT
                                       I01-SEIKYU-STT
                                       I01-SEIKYU-ITM-STT
                                       I01-DISPKBN-STT
                                       I01-DISPKBN-ITM-STT
      *
      *    退院再計算
           WHEN    "10"
               MOVE    WIDGET-INSENSITIVE  TO  WRK-WIDGET-STATE
               PERFORM 5001-SET-WIDGET-STATE-SEC
               MOVE    WIDGET-NORMAL       TO  I01-B12-STT
      *    その他
           WHEN    OTHER
               MOVE    WIDGET-INSENSITIVE  TO  WRK-WIDGET-STATE
               PERFORM 5001-SET-WIDGET-STATE-SEC
               MOVE    WIDGET-NORMAL       TO  I01-B12-STT
                                               I01-USERPG-STT
                                               I01-USERPG-ITM-STT
           END-EVALUATE
      *
           IF    ( SPA-NAI-I01-PTID    =   ZERO )
               MOVE    WIDGET-NORMAL       TO  WRK-WIDGET-STATE
               PERFORM 5001-SET-WIDGET-STATE-SEC
           END-IF
      *
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" )
            AND  ( SPA-GMN-I01-SHOKAI-CD   =   "2" )
               MOVE    WIDGET-NORMAL   TO  I01-SHONUM-STT
               MOVE    WIDGET-NORMAL   TO  I01-KAIGO-STT
           ELSE
               MOVE    WIDGET-INSENSITIVE
                                       TO  I01-KAIGO-STT
                                           I01-KAIGO-ITM-STT
           END-IF
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-GMN-KSN-MAX )
      *
               IF    ( SPA-GMN-I01-KSN-SRYCD (IDX-KSN)
                                           =   SPACE )
                   MOVE    WIDGET-INSENSITIVE
                                       TO  I01-KSN-STT (IDX-KSN)
                                           I01-KSN-ITM-STT (IDX-KSN)
               END-IF
               IF    ( SPA-GMN-I01-CMB-KSN-EDITABLE
                                           =   ZERO )
                   MOVE    WIDGET-INSENSITIVE
                                       TO  I01-KSN-STT (IDX-KSN)
                                           I01-KSN-ITM-STT (IDX-KSN)
               END-IF
      *        
      *        褥瘡患者管理加算の算定済み時は選択コンボ非活性
               IF  (SPA-GMN-I01-KSN-SRYCD(IDX-KSN)  =  "190116990") AND
                   (SPA-I01-JOKUSOUKBN              =  "1"  OR  "2")
                    MOVE    WIDGET-INSENSITIVE
                                       TO   I01-KSN-STT (IDX-KSN)
                                            I01-KSN-ITM-STT(IDX-KSN)
               END-IF
      *
           END-PERFORM
      *
           PERFORM 41011-BUNBEN-NYUIN-CHK-SEC
           IF    ( FLG-BUNBENCHK       =   1 )
            AND  ( SPA-GMN-I01-SYORIKBN-CD
                                       =   "01" OR "03" OR "08" )
                   CONTINUE
           ELSE
                   MOVE    WIDGET-INSENSITIVE
                                       TO   I01-BUNBENKBN-STT
                                            I01-BUNBENKBN-ITM-STT
           END-IF
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" OR "03" OR "08" )
            AND  ( SPA-GMN-I01-BUNBEN-CD   =   "1"  OR "2" )
               CONTINUE
           ELSE
               MOVE    WIDGET-INSENSITIVE
                                       TO   I01-RIYOUKBN-STT
                                            I01-RIYOUKBN-ITM-STT
           END-IF
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "01" OR "08" )
            AND  ( SPA-NAI-I01-TOKHIGAITOCD    NOT =    SPACE )
               CONTINUE
           ELSE
               MOVE    WIDGET-INSENSITIVE
                                       TO  I01-TOKHIGAITO-STT
                                           I01-TOKHIGAITO-ITM-STT
           END-IF
      *
           MOVE    "F"                 TO  I01-TSKJKBN-VISIBLE (1)
                                           I01-TSKJKBN-VISIBLE (2)
                                           I01-TSKJKBN-VISIBLE (3)
           MOVE    WIDGET-INSENSITIVE  TO  I01-TSKJKBN-STT (1)
                                           I01-TSKJKBN-STT (2)
                                           I01-TSKJKBN-STT (3)
                                           I01-TSKJKBN-ITM-STT (1)
                                           I01-TSKJKBN-ITM-STT (2)
                                           I01-TSKJKBN-ITM-STT (3)
      *
           IF    ( SPA-GMN-I01-SKJKBN-CD   =   "B" OR "C" )
               MOVE    "T"             TO  I01-TSKJKBN-VISIBLE (1)
                                           I01-TSKJKBN-VISIBLE (2)
                                           I01-TSKJKBN-VISIBLE (3)
               IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" OR "08" )
                AND  ( SPA-GMN-I01-SKJKBN-CD   =   "C" )
                   MOVE    WIDGET-NORMAL
                                       TO  I01-TSKJKBN-STT (1)
                                           I01-TSKJKBN-STT (2)
                                           I01-TSKJKBN-STT (3)
                                           I01-TSKJKBN-ITM-STT (1)
                                           I01-TSKJKBN-ITM-STT (2)
                                           I01-TSKJKBN-ITM-STT (3)
               END-IF
           END-IF
      *
      *    退院証明書再発行
           IF    ( SPA-GMN-I01-NYUREKILST-SEL >   ZERO )
            AND  ( SPA-GMN-I01-NYUREKILST-SEL
                                  <=   SPA-GMN-I01-NYUREKILST-CNT )
            AND  ( SPA-NAI-I01-TZITA-FLG (SPA-GMN-I01-NYUREKILST-SEL)
                                   =   ZERO )
               MOVE    SPA-GMN-I01-NYUREKILST-SEL
                                   TO   IDX0
               IF    ( SPA-NAI-I01-TTAIINYMD (IDX0)
                                       =   SPACE )
                   IF    ( SPA-NAI-I01-TRRKNUM (IDX0)
                                       =   SPA-NAI-I01-PTRRKNUM )
                    AND  ( SPA-GMN-I01-SYORIKBN-CD =   "02" )
                       MOVE    WIDGET-NORMAL
                                       TO  I01-B10-STT
                   ELSE
                       MOVE    WIDGET-INSENSITIVE
                                       TO  I01-B10-STT
                   END-IF
               ELSE
                   MOVE    WIDGET-NORMAL
                                       TO  I01-B10-STT
               END-IF
           ELSE
               MOVE    WIDGET-INSENSITIVE
                                       TO  I01-B10-STT
           END-IF
      *
      *    出産育児一時金画面
           MOVE    WIDGET-INSENSITIVE  TO  I01-B15-STATE
           IF    ( I01-B15-LABEL   NOT =   SPACE )
               MOVE    WIDGET-NORMAL   TO  I01-B15-STATE
           END-IF
      *
      *
      *    入院オーダー遷移
           MOVE    WIDGET-INSENSITIVE  TO  I01-B24-STATE
           IF    ( I01-B24-LABEL   NOT =   SPACE )
               MOVE    WIDGET-NORMAL   TO  I01-B24-STATE
           END-IF
      *
      *    業務処理権限設定
      *    入院会計照会
           IF    ( SPA-GSRAUTH-32      =   "1" )
               MOVE    WIDGET-NORMAL       TO  I01-B21-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  I01-B21-STATE
           END-IF
      *
      *    患者登録
           IF    ( SPA-GSRAUTH-12      =   "1" )
               MOVE    WIDGET-NORMAL       TO  I01-B05-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  I01-B05-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       5001-SET-WIDGET-STATE-SEC       SECTION.
      *
           MOVE    WRK-WIDGET-STATE    TO  I01-B07-STT
                                           I01-B08-STT
                                           I01-B10-STT
                                           I01-B12-STT
                                           I01-IDOYMD-STT
                                           I01-BRMNUM-STT
                                           I01-BTUNAME-STT
                                           I01-BTUNAME-ITM-STT
                                           I01-SAGAKU-STT
                                           I01-SAGAKU-ITM-STT
                                           I01-SKJKBN-STT
                                           I01-SKJKBN-ITM-STT
                                           I01-TSKJKBN-STT (1)
                                           I01-TSKJKBN-STT (2)
                                           I01-TSKJKBN-STT (3)
                                           I01-TSKJKBN-ITM-STT (1)
                                           I01-TSKJKBN-ITM-STT (2)
                                           I01-TSKJKBN-ITM-STT (3)
                                           I01-TIMEKBN-STT
                                           I01-TIMEKBN-ITM-STT
                                           I01-SENTEIKBN-STT
                                           I01-SENTEIKBN-ITM-STT
                                           I01-NYUINYMD-STT
                                           I01-NYUINKA-STT
                                           I01-NYUINKA-ITM-STT
                                           I01-SHOKAI-STT
                                           I01-SHOKAI-ITM-STT
                                           I01-SHONUM-STT
                                           I01-KAIGO-STT
                                           I01-KAIGO-ITM-STT
                                           I01-TAIINYMD-STT
                                           I01-TAIINCD-STT
                                           I01-TAIINCD-ITM-STT
                                           I01-DR-STT (1)
                                           I01-DR-ITM-STT (1)
                                           I01-DR-STT (2)
                                           I01-DR-ITM-STT (2)
                                           I01-DR-STT (3)
                                           I01-DR-ITM-STT (3)
                                           I01-HKNCOMBI-STT
                                           I01-HKNCOMBI-ITM-STT
                                           I01-TOKNYUIN-STT
                                           I01-TOKNYUIN-ITM-STT
                                           I01-TOKHIGAITO-STT
                                           I01-TOKHIGAITO-ITM-STT
                                           I01-SNTNYUIN-STT
                                           I01-NYUCALKBN-STT
                                           I01-NYUCALKBN-ITM-STT
                                           I01-KSN-STT (1)
                                           I01-KSN-ITM-STT (1)
                                           I01-KSN-STT (2)
                                           I01-KSN-ITM-STT (2)
                                           I01-KSN-STT (3)
                                           I01-KSN-ITM-STT (3)
                                           I01-SEIKYU-STT
                                           I01-SEIKYU-ITM-STT
                                           I01-DISPKBN-STT
                                           I01-DISPKBN-ITM-STT
                                           I01-USERPG-STT
                                           I01-USERPG-ITM-STT
                                           I01-BUNBENKBN-STT
                                           I01-BUNBENKBN-ITM-STT
                                           I01-RIYOUKBN-STT
                                           I01-RIYOUKBN-ITM-STT
      *
           .
       5001-SET-WIDGET-STATE-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC              SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-I01-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-I01-CUR
           WHEN    1
               MOVE    "PTNUM"         TO  MCP-WIDGET
           WHEN    2
               MOVE    "SYORIKBN"      TO  MCP-WIDGET
           WHEN    3
               MOVE    "IDOYMD"        TO  MCP-WIDGET
           WHEN    4
               MOVE    "BRMNUM"        TO  MCP-WIDGET
           WHEN    5
               MOVE    "BTUNAME"       TO  MCP-WIDGET
           WHEN    6
               MOVE    "SAGAKU"        TO  MCP-WIDGET
           WHEN    7
               MOVE    "NYUINYMD"      TO  MCP-WIDGET
           WHEN    8
               MOVE    "NYUINKA"       TO  MCP-WIDGET
           WHEN    9
               MOVE    "SHOKAI"        TO  MCP-WIDGET
           WHEN    10
               MOVE    "SHONUM"        TO  MCP-WIDGET
           WHEN    11
               MOVE    "TAIINYMD"      TO  MCP-WIDGET
           WHEN    12
               MOVE    "DR1"           TO  MCP-WIDGET
           WHEN    13
               MOVE    "DR2"           TO  MCP-WIDGET
           WHEN    14
               MOVE    "DR3"           TO  MCP-WIDGET
           WHEN    15
               MOVE    "HKNCOMBI"      TO  MCP-WIDGET
           WHEN    16
               MOVE    "TOKNYUIN"      TO  MCP-WIDGET
           WHEN    18
               MOVE    "SEIKYU"        TO  MCP-WIDGET
           WHEN    19
               MOVE    "DISPKBN"       TO  MCP-WIDGET
           WHEN    20
               MOVE    "TAIINCD"       TO  MCP-WIDGET
           WHEN    21
               MOVE    "SKJKBN"        TO  MCP-WIDGET
           WHEN    22
               MOVE    "SENTEIKBN"     TO  MCP-WIDGET
           WHEN    24
               MOVE    "USERPG"        TO  MCP-WIDGET
           WHEN    25
               MOVE    "TIMEKBN"       TO  MCP-WIDGET
           WHEN    26
               MOVE    "KAIGO"         TO  MCP-WIDGET
           WHEN    27
               MOVE    "KSN01"         TO  MCP-WIDGET
           WHEN    28
               MOVE    "KSN02"         TO  MCP-WIDGET
           WHEN    29
               MOVE    "KSN03"         TO  MCP-WIDGET
           WHEN    30
               MOVE    "BUNBENKBN"     TO  MCP-WIDGET
           WHEN    31
               MOVE    "RIYOUKBN"      TO  MCP-WIDGET
           WHEN    32
               MOVE    "NYUCALKBN"     TO  MCP-WIDGET
           WHEN    33
               MOVE    "TOKHIGAITO"    TO  MCP-WIDGET
           WHEN    34
               MOVE    "SKJKBN_ASA"    TO  MCP-WIDGET
           WHEN    35
               MOVE    "SKJKBN_HIRU"   TO  MCP-WIDGET
           WHEN    36
               MOVE    "SKJKBN_YU"     TO  MCP-WIDGET
           WHEN    100
               MOVE    "SELNUM"        TO  MCP-WIDGET
           WHEN    206
               MOVE    "B06"           TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "IDOYMD"
               MOVE    004             TO  SPA-GMN-I01-CUR
           WHEN    "BRMNUM"
               MOVE    005             TO  SPA-GMN-I01-CUR
           WHEN    "BTUNAME"
               MOVE    006             TO  SPA-GMN-I01-CUR
           WHEN    "SAGAKU"
               MOVE    022             TO  SPA-GMN-I01-CUR
           WHEN    "NYUINYMD"
               MOVE    025             TO  SPA-GMN-I01-CUR
           WHEN    "NYUINKA"
               MOVE    009             TO  SPA-GMN-I01-CUR
           WHEN    "SHOKAI"
               MOVE    010             TO  SPA-GMN-I01-CUR
           WHEN    "SHONUM"
               MOVE    026             TO  SPA-GMN-I01-CUR
           WHEN    "KAIGO"
               MOVE    011             TO  SPA-GMN-I01-CUR
           WHEN    "TAIINYMD"
               MOVE    020             TO  SPA-GMN-I01-CUR
           WHEN    "DR1"
               MOVE    013             TO  SPA-GMN-I01-CUR
           WHEN    "DR2"
               MOVE    014             TO  SPA-GMN-I01-CUR
           WHEN    "DR3"
               MOVE    015             TO  SPA-GMN-I01-CUR
           WHEN    "HKNCOMBI"
               MOVE    016             TO  SPA-GMN-I01-CUR
           WHEN    "TOKNYUIN"
               MOVE    033             TO  SPA-GMN-I01-CUR
           WHEN    "TOKHIGAITO"
               MOVE    032             TO  SPA-GMN-I01-CUR
           WHEN    "NYUCALKBN"
               MOVE    030             TO  SPA-GMN-I01-CUR
           WHEN    "BUNBENKBN"
               MOVE    031             TO  SPA-GMN-I01-CUR
           WHEN    "RIYOUKBN"
               MOVE    027             TO  SPA-GMN-I01-CUR
           WHEN    "KSN01"
               MOVE    028             TO  SPA-GMN-I01-CUR
           WHEN    "KSN02"
               MOVE    029             TO  SPA-GMN-I01-CUR
           WHEN    "KSN03"
               MOVE    017             TO  SPA-GMN-I01-CUR
           WHEN    "MAEUKE"
               MOVE    018             TO  SPA-GMN-I01-CUR
           WHEN    "SEIKYU"
               MOVE    019             TO  SPA-GMN-I01-CUR
           WHEN    "DISPKBN"
               MOVE    021             TO  SPA-GMN-I01-CUR
           WHEN    "TAIINCD"
               MOVE    012             TO  SPA-GMN-I01-CUR
           WHEN    "TIMEKBN"
               MOVE    008             TO  SPA-GMN-I01-CUR
           WHEN    "SENTEIKBN"
               MOVE    007             TO  SPA-GMN-I01-CUR
           WHEN    "SKJKBN"
               MOVE    034             TO  SPA-GMN-I01-CUR
           WHEN    "SKJKBN_ASA"
               MOVE    035             TO  SPA-GMN-I01-CUR
           WHEN    "SKJKBN_HIRU"
               MOVE    036             TO  SPA-GMN-I01-CUR
           WHEN    "SKJKBN_YU"
               MOVE    024             TO  SPA-GMN-I01-CUR
           WHEN    "USERPG"
               MOVE    212             TO  SPA-GMN-I01-CUR
           WHEN    "SELNUM"
               IF    ( SPA-GMN-I01-SYORIKBN-CD =   "10" )
                   MOVE    212         TO  SPA-GMN-I01-CUR
               ELSE
                   MOVE    206         TO  SPA-GMN-I01-CUR
               END-IF
      *
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC           SECTION.
      *
           IF    ( SPA-GMN-I01-CUR =   3   )
               IF  ( I01-IDOYMD-STT    =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   4
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   4   )
               IF  ( I01-BRMNUM-STT    =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   5
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   5   )
               IF  ( I01-BTUNAME-STT   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   6
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   6   )
               IF  ( I01-SAGAKU-STT    =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   22
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   22   )
               IF  ( I01-SENTEIKBN-STT    =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   7
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   7   )
               IF  ( I01-NYUINYMD-STT  =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   25
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   25   )
               IF  ( I01-TIMEKBN-STT   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   8
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   8   )
               IF  ( I01-NYUINKA-STT   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   9
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   9   )
               IF  ( I01-SHOKAI-STT    =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   10
               END-IF
           END-IF
      *
      *    初回選択番号
           IF    ( SPA-GMN-I01-CUR =   10  )
               IF  ( I01-SHONUM-STT    =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   11
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   11  )
               IF  ( I01-TAIINYMD-STT  =   WIDGET-NORMAL )
                   MOVE    "TAIINYMD"      TO  MCP-WIDGET
                   GO  TO  5012-SKIP-CUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   20
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   20  )
               IF  ( I01-TAIINCD-STT   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   12
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   12  )
               IF  ( I01-DR-STT (1)    =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   13
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   13  )
               IF  ( I01-DR-STT (2)    =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   14
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   14  )
               IF  ( I01-DR-STT (3)    =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   15
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   15  )
               IF  ( I01-HKNCOMBI-STT  =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   16
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   16  )
               IF  ( I01-TOKNYUIN-STT  =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   33
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   33  )
               IF  ( I01-TOKHIGAITO-STT  =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   32
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   32  )
               IF  ( I01-NYUCALKBN-STT     =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   30
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   30  )
               IF  ( I01-BUNBENKBN-STT =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   31
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   31  )
               IF  ( I01-RIYOUKBN-STT  =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   27
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   27  )
               IF  ( I01-KSN-STT (1)   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   28
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   28  )
               IF  ( I01-KSN-STT (2)   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   29
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   29  )
               IF  ( I01-KSN-STT (3)   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   17
               END-IF
           END-IF
      *
      *??? xx 暫定 xx 前受け金にはカーソルを飛ばさないようにする xx
           IF    ( SPA-GMN-I01-CUR =   17  )
               COMPUTE SPA-GMN-I01-CUR =   18
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   18  )
               IF  ( I01-SEIKYU-STT    =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   19
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   19  )
               IF  ( I01-DISPKBN-STT   =   WIDGET-INSENSITIVE )
                   MOVE    21          TO  SPA-GMN-I01-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   21   )
               IF  ( I01-SKJKBN-STT    =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   34
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   34   )
               IF  ( I01-TSKJKBN-STT (1)   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   35
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   35   )
               IF  ( I01-TSKJKBN-STT (2)   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   36
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   36   )
               IF  ( I01-TSKJKBN-STT (3)   =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I01-CUR =   24
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   24  )
               IF  ( I01-USERPG-STT    =   WIDGET-INSENSITIVE )
                   MOVE    212          TO  SPA-GMN-I01-CUR
               END-IF
           END-IF
      *
           .
      *
       5012-SKIP-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "入力エラー"    TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "該当の患者は存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "半角文字でのみ入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "該当の病室は存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "初歴を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "患者番号入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "処理区分入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0013"
               MOVE    "異動日入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0014"
               MOVE    "病室番号入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0015"
               MOVE    "病棟名入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0016"
               MOVE    "室料差額入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0017"
               MOVE    "入院日入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0018"
               MOVE    "入院科入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0019"
               MOVE    "初回入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0020"
               MOVE    "初歴入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0021"
               MOVE    "退院日入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0022"
               MOVE    "担当医入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0023"
               MOVE    "保険組合せ入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0024"
               MOVE    "特定入院料入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0025"
               MOVE    "定期請求入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0026"
               MOVE    "検索時患者表示入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0027"
               MOVE    "選択番号入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0028"
               MOVE    "退院事由入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0029"
               MOVE    "介護異動入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0030"
               MOVE    "入院料加算入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0031"
               MOVE    "分娩入院入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "患者番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "処理区分を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "病室番号を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "病棟名を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE    "入院日を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1006"
               MOVE    "入院科を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1007"
               MOVE    "初回を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    "担当医を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1009"
               MOVE    "保険組合せを入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1010"
               MOVE    "定期請求を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1011"
               MOVE    "検索時患者表示を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1012"
               MOVE    "退院日＜入院日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1013"
               MOVE    "退院日＜前回異動日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1014"
               MOVE    "異動日が入力されていません"
                                       TO  SPA-ERRMSG
           WHEN    "1015"
               MOVE    "異動日＜入院日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1016"
               MOVE    "異動日＜前回異動日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1017"
               MOVE    "入院科・病棟・病室・保険組合せ・特定入院"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "料のいずれかを変更してください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1018"
               MOVE    "入院日＜前回退院日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1019"
               MOVE    "入院歴を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "1020"
               MOVE    "退院日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1021"
               MOVE    "保険組合せがありません。処理できません"
                                       TO  SPA-ERRMSG
           WHEN    "1022"
               MOVE    "継続元の歴番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1023"
               MOVE    "入院歴作成画面で作成した入院歴を選択して"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "ください"
                                       TO  SPA-ERRMSG (41 : )
      *    WHEN    "1024"
      *        MOVE    "選択歴より新しい歴が存在するため修正でき"
      *                                TO  SPA-ERRMSG (1 : 40)
      *        MOVE    "ません"        TO  SPA-ERRMSG (41 : )
           WHEN    "1025"
               MOVE    "定期請求が行われていないため、負担金計算"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "は行えません"  TO  SPA-ERRMSG (41 : )
           WHEN    "1026"
               MOVE    "定期請求済みの期間に該当するため、退院登"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "録は行えません" 
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1027"
               MOVE    "入院料が指定されていません"
                                       TO  SPA-ERRMSG
           WHEN    "1028"
               MOVE    "定期請求が月末のみに変更されています。"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "月途中の入金（請求）を取消してください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1029"
               MOVE    "異動月の入院会計が作成されていません"
                                       TO  SPA-ERRMSG
           WHEN    "1030"
               MOVE    "退院月の入院会計が作成されていません"
                                       TO  SPA-ERRMSG
           WHEN    "1031"
               MOVE    "前歴の自院歴が「入院中」です。前歴の継続"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "入院として登録してください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1032"
               MOVE    "前歴の自院歴が「入院中」です。入院日を前"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "歴の退院日の翌日にしてください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1033"
               MOVE    "労災保険（アフターケア）は入院できません"
                                       TO  SPA-ERRMSG
           WHEN    "1034"
               MOVE    "退院済の歴を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "1035"
               MOVE    "入院歴作成処理で作成した歴は処理の対象外"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "です"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1036"
               MOVE  "入院日≦前回退院日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1037"
               MOVE    "同日再入院の場合、同日の退院登録は行えません"
                                       TO  SPA-ERRMSG
           WHEN    "1038"
               MOVE    "同日再入院は継続入院のみ可能です"
                                       TO  SPA-ERRMSG
           WHEN    "1039"
               MOVE    "病棟管理情報の設定を平成１８年４月１日よ"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "り新規に作成してください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1040"
               MOVE    "医療機関情報−入院基本の設定を平成１８年"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE   "４月１日より新規に作成してください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1041"
               MOVE    "病棟管理情報の設定を平成１８年７月１日よ"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "り新規に作成してください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1042"
               MOVE    "異動月前月の入院会計が作成されていません"
                                       TO  SPA-ERRMSG
           WHEN    "1043"
               MOVE    "異動日を７月１日にし、転科転棟転室処理を"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "行ってください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1044"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  WRK-ERRMSG          DELIMITED   BY  SPACE
                       "有効な医療機関情報−入院基本の設定が"
                                           DELIMITED   BY  SIZE
                       "存在しません"      DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1045"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  WRK-ERRMSG          DELIMITED   BY  SPACE
                       "有効な病棟管理情報が存在しません"
                                           DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1046"
               MOVE    "介護異動を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1047"
               MOVE    "入院日＜誕生日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1048"
               MOVE    "入院会計を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1049"
               MOVE    "入院登録時は選択できない食事区分です"
                                       TO  SPA-ERRMSG
           WHEN    "2008"
               MOVE    "負担金計算に失敗しました"
                                       TO  SPA-ERRMSG
           WHEN    "3000"    THRU      "3999"
      *
               MOVE    SPACE       TO  SPA-ERRMSG
               STRING  "負担金計算に失敗しました。"
                                       DELIMITED BY SIZE
                       S02MSG-MSG (1)  DELIMITED BY SPACE
               INTO    SPA-ERRMSG
               END-STRING
      *
           WHEN    "4001"  THRU    "4036"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
      *
           WHEN    "9300"
               MOVE    "患者登録の業務権限はありません。"
                                                TO  SPA-ERRMSG
               MOVE    "患者登録はできません。"
                                                TO  SPA-ERRMSG(33:)
           WHEN    "9999"
               MOVE    "他端末で使用中です。"
                                       TO  SPA-ERRMSG
               MOVE    SLOCK-MSG       TO  SPA-ERRMSG(21:)
           END-EVALUATE
      *
           MOVE    SPACE               TO  I0ERR
           MOVE    SPA-ERRCD           TO  I0ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  I0ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "I0ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I0ERR"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-I0IDSET-SEC             SECTION.
      *
           EVALUATE    SPA-I0IDCD
           WHEN    "3001"
               MOVE    "入院情報を削除します（会計情報は残ります）"
                                       TO  WRK-I0IDMSG
           WHEN    "3002"
               MOVE    "入院情報を削除します"
                                       TO  WRK-I0IDMSG (1 : 20)
               MOVE    "（会計情報も併せて削除します）"
                                       TO  WRK-I0IDMSG (21 : )
           WHEN    "3003"
               MOVE    "退院取消を行います"
                                       TO  WRK-I0IDMSG
           WHEN    "3004"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "退院後の診療日で入力された診療行為は"
                       "削除されます。確認してください"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "3005"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "定期請求が月末のみに変更されています。"
                       "月途中で作成した請求データは取消します"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "3006"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "入院情報（異動歴あり）を削除します"
                       "（会計情報は残ります）"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "3008"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "定期請求処理中のため退院処理は行えません。"
                       "よろしいですか"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "3007"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "入院情報（異動歴あり）を削除します"
                       "（会計情報も併せて削除します）"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "3009"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "入院日が他院退院日と重複します。"
                       "他院歴の通算日数を減算しますか？"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "3010"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "入院日が他院退院日と重複します。"
                       "よろしいですか？"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "3011"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "退院後の診療日で入力された診療行為は"
                       "そのままの状態となります"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           END-EVALUATE
      *
      *
           MOVE    SPACE                TO  I0ID1
           INITIALIZE                       I0ID1
           MOVE    SPA-I0IDCD           TO  I0ID1-ID1CODE
           MOVE    WRK-I0IDMSG          TO  I0ID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "I01"                TO  SPA-MOTOPG
           MOVE    "I0ID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "I0ID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-I0IDSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-I0ID2SET-SEC            SECTION.
      *
           EVALUATE    SPA-I0IDCD2
           WHEN    "3101"
               MOVE    "転科（転棟・転室）歴を削除します"
                                       TO  WRK-I0IDMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE               TO  I0ID2
           INITIALIZE                      I0ID2
           MOVE    SPA-I0IDCD2         TO  I0ID2-ID2CODE
           MOVE    WRK-I0IDMSG         TO  I0ID2-ID2MSG
      *
      *
           MOVE    SPA-GMN-I01-MAEIDOYMD
                                       TO  I0ID2-IDOYMD
           MOVE    SPA-GMN-I01-NYUINKA-NM
                                       TO  I0ID2-NYUINKA
      *
           MOVE    SPA-GMN-I01-BRMNUM  TO  I0ID2-BRM
      *
           MOVE    SPA-GMN-I01-BTUNAME-NM
                                       TO  I0ID2-BTU
      *
           MOVE    SPA-GMN-I01-HKNCOMBI-NM
                                       TO  I0ID2-HKNCOMBI
           MOVE    SPA-GMN-I01-SNTNYUIN
                                       TO  I0ID2-TOKNYUIN
      *
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "I01"                TO  SPA-MOTOPG
           MOVE    "I0ID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "I0ID2"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-I0ID2SET-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-SYMD
      *
           IF    ( WRK-YMD         =   SPACE )
               GO  TO  5002-HIZUKE-CHK-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間算出処理
      *****************************************************************
       5002-KIKAN-CALC-SEC             SECTION.
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
           IF      ( STS-DAY-RC1   NOT =   ZERO )
               MOVE    1           TO  FLG-ORCSDAY
               GO  TO              5002-KIKAN-CALC-EXT
           END-IF
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
      *
           .
       5002-KIKAN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       5002-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       5002-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-MAE-INPUT
                                   =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    1           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
      *    全角でない場合はエラーとする。
           IF      ( KANACHK-SYUBETU   NOT =   2 )
               MOVE    1           TO  FLG-ENTRYCHK
           END-IF
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    同日再入院チェックサブ
      *****************************************************************
       800-ORCSDNTCHK-SEC              SECTION.
      *
           INITIALIZE                  SDNTCHK-AREA
           MOVE    PTNYUINRRK-REC      TO      LNKNRRK-REC
           CALL    "ORCSDNTCHK"        USING   SDNTCHK-AREA
                                               LNKNRRK-REC
      *
           .
       800-ORCSDNTCHK-EXT.
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
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       800-AGECHK-SEC                  SECTION.
      *
           INITIALIZE ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM          TO  AGECHK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID
                                       TO  AGECHK-PTID
           MOVE    WRK-KJNYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-NAI-I01-BIRTHDAY
                                       TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
           IF    ( AGECHK-RC       =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSAGECHKAREA
           END-IF
      *
           .
      *
       800-AGECHK-EXT.
           EXIT.
      *****************************************************************
      *    病室番号編集処理
      *****************************************************************
       800-ORCSBRMNUM-SEC              SECTION.
      *
           INITIALIZE                  ORCSBRMNUMAREA
           MOVE    "1"             TO  SBRMNUM-IN-PROCKBN
           MOVE    WRK-BRMNUM-IN   TO  SBRMNUM-IN-BRMNUM
           CALL    "ORCSBRMNUM"    USING   ORCSBRMNUMAREA
           MOVE    SBRMNUM-OT-BRMNUM
                                   TO  WRK-BRMNUM-OT
      *
           .
       800-ORCSBRMNUM-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料非該当チェック処理
      *****************************************************************
       800-TOKHIGAITO-CHK-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-TOKHIGAITO-G
      *
           INITIALIZE                      ORCSTOKNYUIN-AREA
           MOVE    WRK-TOKNYUIN-KBN    TO  LNK-TOKNYUIN-KBN
           MOVE    WRK-TOKNYUIN-CD     TO  LNK-TOKNYUIN-NYUIN
           MOVE    SPA-GMN-I01-BTUNAME-CD
                                       TO  LNK-TOKNYUIN-BTUNUM
           MOVE    "1"                 TO  LNK-TOKNYUIN-SANTEIYOUKENKBN
           MOVE    WRK-KJNYMD          TO  LNK-TOKNYUIN-SANTEIYM
           CALL    "ORCSTOKNYUIN"      USING    ORCSTOKNYUIN-AREA
                                                SPA-AREA
           IF    ( LNK-TOKNYUIN-RC          =   ZERO )
            AND  ( LNK-TOKNYUIN-GAI-SRYCD   NOT =  SPACE )
               MOVE    LNK-TOKNYUIN-GAI-SRYCD
                                       TO  WRK-TOKHIGAITO-CD
           END-IF
      *
           .
       800-TOKHIGAITO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       800-HIZUKE-CHK-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-SYMD
      *
           IF    ( WRK-YMD         NOT =   SPACE )
      *
      *        日付画面チェックサブ
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-YMD         TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF  ( SGDAY-RC          ZERO )
                   MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               END-IF
           END-IF
      *
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    WRK-KJNYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       901-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       901-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC       SECTION.
      *
           INITIALIZE              SYSKANRI-REC
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    WRK-KANRICD     TO  SYS-KANRICD
           MOVE    WRK-KBNCD       TO  SYS-KBNCD
           MOVE    WRK-YUKYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY23-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key23"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY23-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY23-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key23"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY23-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY15-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key15"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY15-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY15-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key15"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY15-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY9-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY9-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
      *    患者マスタクローズ
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-NAI-I01-PTID    TO  NACCT-PTID
           MOVE    SPA-NAI-I01-NYUINYMD (1:6)
                                       TO  NACCT-SRYYM
           MOVE    "4"                 TO  NACCT-ZAISKBKBN
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       900-NYUINACCT-KEY17-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY17-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       900-NYUINACCT-KEY7-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY7-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       900-NYUINACCT-KEY7-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY7-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       900-NYUINACCT-KEY19-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SPA-NAI-I01-PTID    TO  NACCT-PTID
           MOVE    "999999"            TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY19-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *    入院診療行為検索
      *****************************************************************
       900-NYUINACT-KEY9-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACT
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM        TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF   (  MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACT-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *****************************************************************
      *    患者入院履歴情報検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO   FLG-PTNYUINRRK
           INITIALIZE  PTNYUINRRK-REC
      *
           MOVE    SPA-HOSPNUM     TO   PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID
                                   TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-I01-PTRRKNUM
                                   TO  PTNYUINRRK-RRKNUM
           MOVE    SPA-NAI-I01-PTRRKEDANUM
                                   TO  PTNYUINRRK-RRKEDANUM
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
      *
           MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNYUINRRK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者入院履歴情報検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY75-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO   FLG-PTNYUINRRK
           INITIALIZE  PTNYUINRRK-REC
      *
           MOVE    SPA-HOSPNUM     TO   PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-I01-PTID
                                   TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-I01-PTRRKNUM
                                   TO  PTNYUINRRK-RRKNUM
           MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  PTNYUINRRK-NYUINYMD
                                       PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
      *
           MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
           MOVE    "key75"            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key75"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNYUINRRK-KEY75-SEL-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
           INITIALIZE                  BUNBEN-REC
      *
           MOVE    SPA-HOSPNUM     TO  BUNBEN-HOSPNUM
           MOVE    SPA-NAI-I01-PTID
                                   TO  BUNBEN-PTID
           MOVE    "1"             TO  BUNBEN-NYUGAIKBN
           MOVE    WRK-BUNBENRRKNUM
                                   TO  BUNBEN-RRKNUM
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
      *    入院料加算チェックテーブル検索処理(KEY)
      *****************************************************************
       900-NYUKSNCHK-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUKSNCHK
      *
           INITIALIZE                      NYUKSNCHK-REC
           MOVE    WRK-NYUKSNKBN       TO  NYUKSNCHK-NYUINKBN
           MOVE    WRK-KSN-NYUKSNKBN   TO  NYUKSNCHK-KSNKBN
           MOVE    WRK-KJNYMD          TO  NYUKSNCHK-YUKOSTYMD
                                           NYUKSNCHK-YUKOEDYMD
           MOVE    NYUKSNCHK-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuksnchk"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUKSNCHK-REC
           ELSE
               INITIALIZE                  NYUKSNCHK-REC
               MOVE    1               TO  FLG-NYUKSNCHK
           END-IF
      *
           MOVE    "tbl_nyuksnchk"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUKSNCHK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為照会検索処理
      *****************************************************************
       900-NSRYSRH-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-NSRYSRH
      *
           INITIALIZE                  NSRYSRH-REC
      *
           MOVE    SPA-HOSPNUM     TO  NSRYSRH-HOSPNUM
           MOVE    SPA-NAI-I01-PTID
                                   TO  NSRYSRH-PTID
           MOVE    NRRKSRH-ZAINUM  TO  NSRYSRH-ZAINUM
           MOVE    NRRKSRH-SRYYMD(1:6)
                                   TO  NSRYSRH-SRYYM
           MOVE    NSRYSRH-REC     TO  MCPDATA-REC
      *
           MOVE    "tbl_nsrysrh"    TO  MCP-TABLE
           MOVE    "key"            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  NSRYSRH-REC
           ELSE
               INITIALIZE              NSRYSRH-REC
               MOVE    1           TO  FLG-NSRYSRH
           END-IF
      *
           MOVE    "tbl_nsrysrh"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-NSRYSRH-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴照会検索処理
      *****************************************************************
       900-NRRKSRH-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-NRRKSRH
      *
           INITIALIZE                  NRRKSRH-REC
      *
           MOVE    SPA-HOSPNUM     TO  NRRKSRH-HOSPNUM
           MOVE    SPA-NAI-I01-PTID
                                   TO  NRRKSRH-PTID
           MOVE    SPA-NAI-I01-PTRRKNUM
                                   TO  NRRKSRH-RRKNUM
           MOVE    WRK-KEISANYMD   TO  NRRKSRH-SRYYMD
           MOVE    NRRKSRH-REC     TO  MCPDATA-REC
           MOVE    "tbl_nrrksrh"    TO  MCP-TABLE
           MOVE    "key2"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  NRRKSRH-REC
           ELSE
               INITIALIZE              NRRKSRH-REC
               MOVE    1           TO  FLG-NRRKSRH
           END-IF
      *
           .
      *
       900-NRRKSRH-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴照会検索処理
      *****************************************************************
       900-NRRKSRH-KEY2-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-NRRKSRH
      *
           MOVE    "tbl_nrrksrh"    TO  MCP-TABLE
           MOVE    "key2"           TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  NRRKSRH-REC
           ELSE
               INITIALIZE              NRRKSRH-REC
               MOVE    1           TO  FLG-NRRKSRH
           END-IF
      *
           .
      *
       900-NRRKSRH-KEY2-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個別設定テーブル読込処理
      *****************************************************************
       900-PTCONF-READ-SEC         SECTION.
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTCONF-REC
                   MOVE    ZERO        TO  FLG-PTCONF
               ELSE
                   INITIALIZE              PTCONF-REC
                   MOVE    1           TO  FLG-PTCONF
               END-IF
           ELSE
               INITIALIZE                  PTCONF-REC
               MOVE    1               TO  FLG-PTCONF
           END-IF
      *
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
       900-PTCONF-READ-EXT.
           EXIT.
      *****************************************************************
      *    収納明細検索処理
      *****************************************************************
       900-SYUMEI-KEY14-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
               INITIALIZE                  SYUMEI-REC
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYUMEI-KEY14-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-NAI-I01-PTID    TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF    ( SLOCK-RETURN    NOT =   ZERO )
               MOVE    "9999"          TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

