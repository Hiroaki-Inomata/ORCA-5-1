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
      ************************************************************
      *         システム管理マスタ     (CPSK200501.INC)          *
      *         （２００５の区分コード０１のとき）               *
      *         レセプト編集区分                                 *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.03.01    NACL-藤原    08/06/19  医師別出力対応
      *  04.03.02    NACL-藤原    08/08/07  労災コメント記載区分
      *                                     入院料未算定時コメント記載区分
      *                                     労災枠記載区分の追加
      *
      *  04.04.01    NACL-藤原    09/11/19  レセ電ＣＤ−Ｒ出力対応
      *  04.04.02    NACL-藤原    09/11/25  光ディスク等送付書作成区分、
      *                                     症状詳記作成区分追加
      *
      *  04.05.01    NACL-藤原    09/06/18  病棟別出力対応
      *  04.05.02    NACL-藤原    09/08/17  生保等まとめ入力画面表示順
      *                                     設定対応
      *  04.05.02    NACL-藤原    09/08/19  全件印刷時の自費レセプトの
      *                                     印刷指定対応
      *  04.05.03    NACL-藤原    09/08/21  レセ電データ格納フォルダ設定
      *                                     対応
      *                                     ファイル出力先のデフォルト値
      *                                     の設定も可能にする
      *  04.05.04    NACL-藤原    11/03/25  災害対応
      *                                     災害該当レセプトのレセ電作成
      *                                     区分の追加
      *  04.05.05    NACL-藤原    11/08/02  労災新様式対応
      *                                     点検レセ印字位置変更対応
      *
      *  04.06.01    NACL-藤原    10/06/23  レセ電請求時の受付エラー分の
      *                                     種別不明レセ対応
      *  04.06.02    NACL-藤原    10/07/10  病名記載順設定対応
      *  04.06.03    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.04    NACL-藤原    12/07/04  表示期間によるレセプト記載対応
      *                                     転帰済傷病名記載区分の追加
      *  04.06.05    NACL-藤原    12/08/20  点検用・提出用初期表示設定対応
      *  04.06.06    NACL-藤原    12/12/21  入院料の算定がない入退院履歴
      *                                     の記載対応
      *                                     入退院履歴の記載区分（入院）
      *                                     の追加
      *
      *  04.07.01    NACL-藤原    12/08/01  レセ電データチェック機能対応
      *  04.07.02    NACL-藤原    12/09/24  点検用レセプトの医師名記載対応
      *  04.07.03    NACL-藤原    12/11/21  ターミナルデジットによる出力対応
      *  04.07.04    NACL-藤原    12/11/21  労災レセ電作成対応
      *  04.07.05    NACL-藤原    13/06/26  入院料他保険算定時コメント記載
      *                                     対応
      *  04.07.06    NACL-藤原    13/12/04  自賠責（労災準拠）のレセプト
      *                                     様式追加対応
      *  04.07.07    NACL-藤原    14/03/20  上限０円時の食事欄記載対応
      *                                    （法別２４・７９）
      *  04.07.08    NACL-藤原    15/05/14  ０点全国公費レセプト記載区分
      *                                    （提出用レセプト）の追加
      *  04.07.09    NACL-藤原    15/06/18  光ディスク等送付書の媒体種類
      *                                     設定対応
      *  04.07.10    NACL-藤原    15/07/17  労災診療費請求書及びアフター
      *                                     ケア委託費請求書のＯＣＲ様式
      *                                     対応
      *
      *  04.08.01    NACL-藤原    12/01/08  包括診療分の点検用レセプト記
      *                                     載対応
      *  04.08.02    NACL-藤原    15/08/17  自賠責レセプトの請求金額欄端
      *                                     数処理対応
      *  04.08.03    NACL-藤原    15/09/01  保険履歴の記載対応
      *  04.08.04    NACL-藤原    15/11/30  現物給付一部負担金記載対応
      *                                    （法別２４・７９）
      *  04.08.05    NACL-藤原    16/01/18  点検用レセプト一般名処方記載
      *                                     対応
      *  04.08.06    NACL-藤原    16/04/20  湿布薬用法レセプト記載対応
      *  04.08.07    NACL-藤原    16/06/13  社保診療報酬請求書の災害該
      *                                     当分のまとめ対応
      *                                     １５円以下薬剤のレセプト記載対応
      *                                     災害該当分（非減免対象と減免対象）
      *                                     レセプトの分割対応
      *  04.08.08    NACL-藤原    16/10/07  自賠責ＰＤＦファイル作成対応
      *  04.08.09    NACL-藤原    18/01/26  全国公費一部負担金記載対応
      *                                    （公費のみのレセプト）
      *
      *  05.00.01    NACL-藤原    19/04/16  元号改正対応
      ************************************************************
       01  SYS-200501-REC.
           03  SYS-200501-KEY.
               05  SYS-200501-HOSPNUM      PIC 9(2).
      *管理コード
               05  SYS-200501-KANRICD      PIC X(04).
      *区分コード
               05  SYS-200501-KBNCD        PIC X(08).
      *有効開始年月日
               05  SYS-200501-STYUKYMD     PIC X(08).
      *有効終了年月日
               05  SYS-200501-EDYUKYMD     PIC X(08).
      *管理テーブル
           03  SYS-200501-TBL.
      *自賠責の様式選択機能（従来様式・入院外）
               05  SYS-200501-JIBAIPRTKBN  PIC X(01).
      *自賠責の様式選択機能（従来様式・入院）
               05  SYS-200501-JIBAIPRTKBN-N
                                           PIC X(01).
      *病棟番号記載区分（入院）
               05  SYS-200501-BTUNUM-KSIKBN
                                           PIC X(01).
      *印刷処理後のチェック解除の有無
               05  SYS-200501-PRT-CHECKKBN PIC X(01).
      *レセプト一括作成出力順区分（広域連合）
               05  SYS-200501-PRTJYUNKBN5  PIC X(02).
      *広域連合ＯＣＲ出力区分
               05  SYS-200501-KOUIKIOCRKBN PIC X(01).
      *広域連合作成区分
               05  SYS-200501-KOUIKIKBN    PIC X(01).
      *広域連合レセ電出力区分
               05  SYS-200501-KOUIKIRECEKBN
                                           PIC X(01).
      *レセ電日計表レコード記録条件（広域連合）
               05  SYS-200501-NI-JYOKEN-AREA.  
                   07  SYS-200501-NI-JYOKEN
                                           PIC 9(08)   OCCURS  2.
      *広域連合出力先プリンタ名
               05  SYS-200501-KOUIKIPRTNM  PIC X(16).
      *広域連合出力先プリンタ名（続紙）
               05  SYS-200501-KOUIKIPRTNM-NEXT
                                           PIC X(16).
      *広域連合出力先プリンタ名（入院）
               05  SYS-200501-KOUIKIPRTNM-N
                                           PIC X(16).
      *広域連合出力先プリンタ名（続紙）（入院）
               05  SYS-200501-KOUIKIPRTNM-NEXT-N
                                           PIC X(16).
      *広域連合診療報酬請求書区分
               05  SYS-200501-KOUIKIPRTKBN PIC X(01).
      *医師別出力区分
               05  SYS-200501-DRCD-PRTKBN  PIC X(01).
      *医師別出力順区分
               05  SYS-200501-DRCD-PRTJYUNKBN
                                           PIC X(01).
      *枠記載区分（労災・入院外）
               05  SYS-200501-FRAME-ROUPRTKBN
                                           PIC X(01).
      *枠記載区分（労災・入院）
               05  SYS-200501-FRAME-ROUPRTKBN-N
                                           PIC X(01).
      *労災コメント記載区分
               05  SYS-200501-COMMENT-ROU-KSIKBN
                                           PIC X(01).
      *入院料未算定時コメント記載区分
               05  SYS-200501-COMMENT-NYUINRYO-KSIKBN
                                           PIC X(01).
      *レセ電ＣＤ−Ｒ出力区分
               05  SYS-200501-RECEDEN-CDRKBN
                                           PIC X(01).
      *症状詳記作成区分
               05  SYS-200501-RECEDEN-LST1KBN
                                           PIC X(01).
      *光ディスク等送付書作成区分
               05  SYS-200501-RECEDEN-LST2KBN
                                           PIC X(01).
      *病棟別出力区分
               05  SYS-200501-BTUNUM-PRTKBN
                                           PIC X(01).
      *病棟別出力順区分
               05  SYS-200501-BTUNUM-PRTJYUNKBN
                                           PIC X(01).
      *レセプト一括作成出力順区分（全体）入院
               05  SYS-200501-PRTJYUNKBN6  PIC X(02).
      *生保等まとめ入力画面表示順
               05  SYS-200501-SEIHO-DSPKBN PIC X(01).
      *自費レセプト印刷区分
               05  SYS-200501-JIHI-PRTKBN  PIC X(01).
      *レセ電データ履歴保存設定区分
               05  SYS-200501-RECEDEN-RIREKIKBN
                                           PIC X(01).
      *ファイル出力先設定区分
               05  SYS-200501-RECEDEN-DATAKBN
                                           PIC X(01).
      *災害該当レセプトのレセ電作成区分
               05  SYS-200501-RECEDEN-DISASTER
                                           PIC X(01).
      *チェック区分
               05  SYS-200501-CHKKBN       PIC X(01).
      *病名記載順区分
               05  SYS-200501-BYOMEI-PRTJYUN
                                           PIC X(01).
      *点検用レセプト記載設定区分
               05  SYS-200501-INSPECT-CHARACTER-KBN
                                           PIC X(01).
      *労災の様式選択機能（入院外）
               05  SYS-200501-ROUPRTKBN    PIC X(01).
      *労災の様式選択機能（入院）
               05  SYS-200501-ROUPRTKBN-N  PIC X(01).
      *点検用レセプト０点公費記載区分
               05  SYS-200501-INSPECT-ZERO-KBN
                                           PIC X(01).
      *転帰済傷病名記載区分
               05  SYS-200501-TENKIBYOMEI-KBN
                                           PIC X(01).
      *レセプト作成区分（点検用・提出用）
               05  SYS-200501-RECEIPT-MAKE-KBN
                                           PIC X(01).
      *レセ電データチェック区分
               05  SYS-200501-RECEDEN-CHK-KBN
                                           PIC X(01).
      *医師名記載区分
               05  SYS-200501-DRNAME-KSIKBN
                                           PIC X(01).
      *ターミナルデジット出力区分
               05  SYS-200501-TERMINAL-DIGIT-PRTKBN
                                           PIC X(01).
      *労災レセ電出力区分
               05  SYS-200501-ROURECEKBN   PIC X(01).
      *入退院履歴記載区分（入院）
               05  SYS-200501-NYUTAIINREKI-NYU-KSIKBN
                                           PIC X(01).
      *点検用レセプト包括算定分記載区分
               05  SYS-200501-INSPECT-INCLUDE-KBN
                                           PIC X(01).
      *入院料他保険算定時コメント記載区分
               05  SYS-200501-COMMENT-NYUINRYO1-KSIKBN
                                           PIC X(01).
      *自賠責の様式選択機能（新様式・入院外）
               05  SYS-200501-JIBAIPRTKBN1 PIC X(01).
      *自賠責の様式選択機能（新様式・入院）
               05  SYS-200501-JIBAIPRTKBN1-N
                                           PIC X(01).
      *食事欄記載区分
               05  SYS-200501-SHOKUJI-PRTKBN
                                           PIC X(01).
      *０点全国公費レセプト記載区分（提出用レセプト）
               05  SYS-200501-ZERO-KOH-PRTKBN
                                           PIC X(01).
      *光ディスク等送付書（媒体種類）
               05  SYS-200501-RECEDEN-LST2-MEDIA
                                           PIC X(01).
      *労災診療費請求書、アフターケア委託費請求書の様式選択
               05  SYS-200501-ROUSAIPRTKBN PIC X(01).
      *労災診療費請求書、アフターケア委託費請求書の枠記載区分
               05  SYS-200501-FRAME-ROUSAIPRTKBN
                                           PIC X(01).
      *自賠責請求金額欄端数処理区分
               05  SYS-200501-JIBAI-FRACTION-KBN
                                           PIC X(01)   OCCURS  2.
      *保険履歴記載区分
               05  SYS-200501-HKN-RIREKI-KBN
                                           PIC X(01).
      *現物給付一部負担金記載
               05  SYS-200501-GENBUTU-ICHIBU-KSIKBN
                                           PIC X(01).
      *点検用レセプト一般名処方記載区分
               05  SYS-200501-INSPECT-GENERAL-KBN
                                           PIC X(01).
      *湿布薬用法レセプト記載区分
               05  SYS-200501-COMPRESS-KBN PIC X(01).
      *社保診療報酬請求書の災害該当分まとめ機能
               05  SYS-200501-DISASTER-SYAPRTKBN
                                           PIC X(01).
      *１５円以下薬剤のレセプト記載区分
               05  SYS-200501-15-YKZ-PRTKBN
                                           PIC X(01).
      *レセプトの災害該当分（非減免対象と減免対象）分割機能
               05  SYS-200501-DISASTER-RECEIPT
                                           PIC X(01).
      *自賠責ＰＤＦファイル作成
               05  SYS-200501-JIBAI-PDF    PIC X(01).
      *全国公費一部負担金記載区分（公費のみのレセプト）
               05  SYS-200501-KOUHI-ICHIBU-KSIKBN
                                           PIC X(01).
      *労災レセプト様式区分
               05  SYS-200501-RSI-RECEIPT-KBN
                                           PIC X(01).
      *労災診療費請求書様式区分
               05  SYS-200501-RSI-INVOICE-KBN
                                           PIC X(01).
      *予備
           03  SYS-200501-YOBI             PIC X(357).
      *端末ＩＤ
           03  SYS-200501-TERMID           PIC X(16).
      *オペーレータＩＤ
           03  SYS-200501-OPID             PIC X(16).
      *作成年月日
           03  SYS-200501-CREYMD           PIC X(08).
      *更新年月日
           03  SYS-200501-UPYMD            PIC X(08).
      *更新時間
           03  SYS-200501-UPHMS            PIC X(06).
      *主キー更新用項目
           03  SYS-200501-UPD-KEY.
               05  SYS-200501-UPD-HOSPNUM     PIC 9(2).
               05  SYS-200501-UPD-KANRICD     PIC X(04).
               05  SYS-200501-UPD-KBNCD       PIC X(08).
               05  SYS-200501-UPD-STYUKYMD    PIC X(08).
               05  SYS-200501-UPD-EDYUKYMD    PIC X(08).
