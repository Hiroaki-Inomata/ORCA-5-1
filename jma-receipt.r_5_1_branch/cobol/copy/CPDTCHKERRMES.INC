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
      *    データチェックエラーメッセージ COPY (CPDTCHKERRMES.INC)*
      *    ※ERRCDの変更は不可！！                                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ERRMES-AREA.
      *
         03  ERRMES-VAL.
      *
      *    ** 01xxxx 保険者番号抜け
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "010001".
           05 PIC  X(30)   VALUE   "保険者番号が未入力です        ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 02xxxx 被保険者記号・番号抜け
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020001".
           05 PIC  X(30)   VALUE   "保険情報の記号が未入力です    ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020002".
           05 PIC  X(30)   VALUE   "保険情報の記号に全角文字以外が".
           05 PIC  X(30)   VALUE   "入力されています              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020003".
           05 PIC  X(30)   VALUE   "保険情報の番号に全角文字以外が".
           05 PIC  X(30)   VALUE   "入力されています              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020004".
           05 PIC  X(30)   VALUE   "レセ電算データの場合、保険情報".
           05 PIC  X(30)   VALUE   "の記号・番号の文字数の和が１９".
           05 PIC  X(30)   VALUE   "文字を超えています            ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020005".
           05 PIC  X(30)   VALUE   "保険情報の番号が未入力です    ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020006".
           05 PIC  X(30)   VALUE   "保険情報の記号に外字が入力され".
           05 PIC  X(30)   VALUE   "ています                      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020007".
           05 PIC  X(30)   VALUE   "保険情報の番号に外字が入力され".
           05 PIC  X(30)   VALUE   "ています                      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020008".
           05 PIC  X(30)   VALUE   "レセ電算データの場合、保険情報".
           05 PIC  X(30)   VALUE   "の記号に空白が入力されています".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020009".
           05 PIC  X(30)   VALUE   "保険情報の記号が入力誤りです  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020010".
           05 PIC  X(30)   VALUE   "保険情報の番号の１文字目が入力".
           05 PIC  X(30)   VALUE   "誤りです                      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020011".
           05 PIC  X(30)   VALUE   "保険情報の番号が入力誤りです  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020012".
           05 PIC  X(30)   VALUE   "保険情報の記号に入力があります".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
      *
      *    ** 03xxxx 公費負担者番号・受給者番号抜け
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030001".
           05 PIC  X(30)   VALUE   "公費情報の負担者番号が未入力で".
           05 PIC  X(30)   VALUE   "す                            ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030002".
           05 PIC  X(30)   VALUE   "公費情報の受給者番号が未入力で".
           05 PIC  X(30)   VALUE   "す                            ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030003".
           05 PIC  X(30)   VALUE   "公費情報の負担者番号に全角文字".
           05 PIC  X(30)   VALUE   "と半角文字が混在しています    ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030004".
           05 PIC  X(30)   VALUE   "公費情報の受給者番号に全角文字".
           05 PIC  X(30)   VALUE   "と半角文字が混在しています    ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030005".
           05 PIC  X(30)   VALUE   "公費情報の負担者番号に外字が入".
           05 PIC  X(30)   VALUE   "力されています                ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030006".
           05 PIC  X(30)   VALUE   "公費情報の受給者番号に外字が入".
           05 PIC  X(30)   VALUE   "力されています                ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030007".
           05 PIC  X(30)   VALUE   "公費情報の負担者番号に半角文字".
           05 PIC  X(30)   VALUE   "が入力されています            ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030008".
           05 PIC  X(30)   VALUE   "公費情報の負担者番号に全角文字".
           05 PIC  X(30)   VALUE   "が入力されています            ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030009".
           05 PIC  X(30)   VALUE   "公費情報の受給者番号に半角文字".
           05 PIC  X(30)   VALUE   "が入力されています            ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030010".
           05 PIC  X(30)   VALUE   "公費情報の受給者番号に全角文字".
           05 PIC  X(30)   VALUE   "が入力されています            ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030011".
           05 PIC  X(30)   VALUE   "公費情報の負担者番号が入力誤り".
           05 PIC  X(30)   VALUE   "です                          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030012".
           05 PIC  X(30)   VALUE   "公費情報の受給者番号が入力誤り".
           05 PIC  X(30)   VALUE   "です                          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030013".
           05 PIC  X(30)   VALUE   "公費情報の受給者番号に入力があ".
           05 PIC  X(30)   VALUE   "ります                        ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 04xxxx 保険適応年齢
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "040001".
           05 PIC  X(30)   VALUE   "１５歳未満ですが本人で登録され".
           05 PIC  X(30)   VALUE   "ています                      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "040002".
           05 PIC  X(30)   VALUE   "後期高齢者の保険適応年齢誤りで".
           05 PIC  X(30)   VALUE   "す                            ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "040003".
           05 PIC  X(30)   VALUE   "後期高齢者の保険が未設定です  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "040004".
           05 PIC  X(30)   VALUE   "後期高齢者の負担割合が２割です".
           05 PIC  X(30)   VALUE   "。３割に登録し直して下さい    ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "040005".
           05 PIC  X(30)   VALUE   "６５歳以上で退職者国保の入力が".
           05 PIC  X(30)   VALUE   "あります                      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "040006".
           05 PIC  X(30)   VALUE   "前期高齢者医療の適用年齢に到達".
           05 PIC  X(30)   VALUE   "しています。保険確認を行ってく".
           05 PIC  X(30)   VALUE   "ださい                        ".
           05 PIC  X(30)   VALUE   "                              ".
      *    ** 05xxxx 患者病名
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050001".
           05 PIC  X(30)   VALUE   "病名が未登録です              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050002".
           05 PIC  X(30)   VALUE   "診療年月に有効な入院病名が存在".
           05 PIC  X(30)   VALUE   "しません                      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050003".
           05 PIC  X(30)   VALUE   "診療年月に有効な外来病名が存在".
           05 PIC  X(30)   VALUE   "しません                      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050004".
           05 PIC  X(30)   VALUE   "患者病名情報テーブルの病名が未".
           05 PIC  X(30)   VALUE   "設定です                      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050005".
           05 PIC  X(30)   VALUE   "病名に全角文字以外が入力されて".
           05 PIC  X(30)   VALUE   "います                        ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050006".
           05 PIC  X(30)   VALUE   "患者病名情報テーブルの病名文字".
           05 PIC  X(30)   VALUE   "数が未設定です                ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050007".
           05 PIC  X(30)   VALUE   "病名に対応する病名コードが入力".
           05 PIC  X(30)   VALUE   "されていません                ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050008".
           05 PIC  X(30)   VALUE   "転帰区分（転帰日）が設定されて".
           05 PIC  X(30)   VALUE   "いる場合、転帰日（転帰区分）も".
           05 PIC  X(30)   VALUE   "設定されていなければなりません".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050009".
           05 PIC  X(30)   VALUE   "カルテ病名に全角文字以外が入力".
           05 PIC  X(30)   VALUE   "されています                  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050010".
           05 PIC  X(30)   VALUE   "レセ電算データの場合、病名文字".
           05 PIC  X(30)   VALUE   "数が１７文字を超えています    ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050011".
           05 PIC  X(30)   VALUE   "レセ電算データの場合、病名文字".
           05 PIC  X(30)   VALUE   "数が２０文字を超えています    ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050012".
           05 PIC  X(30)   VALUE   "同じ病名が複数登録されています".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050013".
           05 PIC  X(30)   VALUE   "主病名が未登録です            ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050014".
           05 PIC  X(30)   VALUE   "病名の診療開始日が誕生日よりも".
           05 PIC  X(30)   VALUE   "前の日付で登録されています    ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050015".
           05 PIC  X(30)   VALUE   "診療年月に保険請求対象外の病名".
           05 PIC  X(30)   VALUE   "のみ登録されています          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 06xxxx 診療開始日
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "060001".
           05 PIC  X(30)   VALUE   "初診算定日から開始になる病名が".
           05 PIC  X(30)   VALUE   "存在しません                  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "060002".
           05 PIC  X(30)   VALUE   "初診算定日以前より継続中の病名".
           05 PIC  X(30)   VALUE   "が存在します                  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "060003".
           05 PIC  X(30)   VALUE   "初診料が算定可能か確認してくだ".
           05 PIC  X(30)   VALUE   "さい                          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "060004".
           05 PIC  X(30)   VALUE   "初診算定日と病名の転帰日が同じ".
           05 PIC  X(30)   VALUE   "日付です                      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "060005".
           05 PIC  X(30)   VALUE   "当月開始病名のみの入力です。初".
           05 PIC  X(30)   VALUE   "診料が算定可能か確認してくださ".
           05 PIC  X(30)   VALUE   "い                            ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "060006".
           05 PIC  X(30)   VALUE   "当月の病名の診療開始日が正しく".
           05 PIC  X(30)   VALUE   "入力されているか確認してくださ".
           05 PIC  X(30)   VALUE   "い。                          ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "060007".
           05 PIC  X(30)   VALUE   "診療日に継続中の病名が存在しま".
           05 PIC  X(30)   VALUE   "せん                          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 07xxxx 急性病名・疑い病名
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "070001".
           05 PIC  X(30)   VALUE   "疑い病名が長期間継続しています".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "070002".
           05 PIC  X(30)   VALUE   "急性病名が長期間継続しています".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 08xxxx 薬剤と病名
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "080001".
           05 PIC  X(30)   VALUE   "薬剤の適応病名未入力です      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "080002".
           05 PIC  X(30)   VALUE   "適応病名が外来の病名として入力".
           05 PIC  X(30)   VALUE   "されています                  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "080003".
           05 PIC  X(30)   VALUE   "適応病名が入院の病名として入力".
           05 PIC  X(30)   VALUE   "されています                  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "080004".
           05 PIC  X(30)   VALUE   "適応病名が有効な期間で登録され".
           05 PIC  X(30)   VALUE   "ていません                    ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 09xxxx 診療行為と病名
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "090001".
           05 PIC  X(30)   VALUE   "診療行為の適応病名未入力です  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "090002".
           05 PIC  X(30)   VALUE   "適応病名が外来の病名として入力".
           05 PIC  X(30)   VALUE   "されています                  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "090003".
           05 PIC  X(30)   VALUE   "適応病名が入院の病名として入力".
           05 PIC  X(30)   VALUE   "されています                  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "090004".
           05 PIC  X(30)   VALUE   "適応病名が有効な期間で登録され".
           05 PIC  X(30)   VALUE   "ていません                    ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 10xxxx 併用禁忌の薬剤
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "100001".
           05 PIC  X(30)   VALUE   "併用禁忌の薬剤が入力されていま".
           05 PIC  X(30)   VALUE   "す                            ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "100002".
           05 PIC  X(30)   VALUE   "併用禁忌の薬剤が入力されていま".
           05 PIC  X(30)   VALUE   "す（相互作用マスタより）      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 11xxxx 併用算定の診療行為
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "110001".
           05 PIC  X(30)   VALUE   "診療行為が併用算定誤りです（同".
           05 PIC  X(30)   VALUE   "月）                          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "110002".
           05 PIC  X(30)   VALUE   "診療行為が併用算定誤りです（同".
           05 PIC  X(30)   VALUE   "日）                          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "110003".
           05 PIC  X(30)   VALUE   "診療行為が併用算定誤りです（同".
           05 PIC  X(30)   VALUE   "会計）                        ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 12xxxx 実日数
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "120001".
           05 PIC  X(30)   VALUE   "実日数を越えた回数を算定してい".
           05 PIC  X(30)   VALUE   "ます                          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "120002".
           05 PIC  X(30)   VALUE   "入院していない日に入院診療があ".
           05 PIC  X(30)   VALUE   "ります                        ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "120003".
           05 PIC  X(30)   VALUE   "外泊日に調剤料が算定されていま".
           05 PIC  X(30)   VALUE   "す                            ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "120004".
           05 PIC  X(30)   VALUE   "実日数がゼロ日です。          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "120005".
           05 PIC  X(30)   VALUE   "外泊日に調剤技術基本料が算定さ".
           05 PIC  X(30)   VALUE   "れています                    ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 13xxxx 患者氏名
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "130001".
           05 PIC  X(30)   VALUE   "漢字氏名に全角文字以外が入力さ".
           05 PIC  X(30)   VALUE   "れています          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "130002".
           05 PIC  X(30)   VALUE   "カナ氏名に全角カタカナ以外の文".
           05 PIC  X(30)   VALUE   "字が入力されています          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "130003".
           05 PIC  X(30)   VALUE   "漢字氏名が未入力です          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "130004".
           05 PIC  X(30)   VALUE   "カナ氏名が未入力です          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "130005".
           05 PIC  X(30)   VALUE   "漢字氏名に外字が入力されていま".
           05 PIC  X(30)   VALUE   "す                            ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "130006".
           05 PIC  X(30)   VALUE   "カナ氏名に外字が入力されていま".
           05 PIC  X(30)   VALUE   "す                            ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "130007".
           05 PIC  X(30)   VALUE   "カナ氏名に労災レセ電データに記".
           05 PIC  X(30)   VALUE   "録できない文字が入力されていま".
           05 PIC  X(30)   VALUE   "す。                          ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 14xxxx 保険組合せ有効期間
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "140001".
           05 PIC  X(30)   VALUE   "保険組合せが存在しません      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "140002".
           05 PIC  X(30)   VALUE   "保険の適用が切れている期間があ".
           05 PIC  X(30)   VALUE   "ります                        ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "140003".
           05 PIC  X(30)   VALUE   "地方公費のみの保険組合せです  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "140004".
           05 PIC  X(30)   VALUE   "保険組合せが正しく作成されてい".
           05 PIC  X(30)   VALUE   "ません。保険または公費情報を確".
           05 PIC  X(30)   VALUE   "認してください                ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 15xxxx 投与禁忌薬剤と病名
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "150001".
           05 PIC  X(30)   VALUE   "投与禁忌薬剤が入力されています".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 16xxxx 診療行為どうしの算定漏れ
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "160001".
           05 PIC  X(30)   VALUE   "該当の診療行為に対して同日の算".
           05 PIC  X(30)   VALUE   "定漏れがあります              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "160002".
           05 PIC  X(30)   VALUE   "該当の診療行為に対して同月の算".
           05 PIC  X(30)   VALUE   "定漏れがあります              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 17xxxx 病名と薬剤
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "170001".
           05 PIC  X(30)   VALUE   "薬剤が未入力です              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 18xxxx 病名と診療行為
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "180001".
           05 PIC  X(30)   VALUE   "診療行為が未入力です          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *    ** 19xxxx 薬剤の月上限回数・総投与量
           05 PIC  X(06)   VALUE   "190001".
           05 PIC  X(30)   VALUE   "月の投与回数または投与量が上限".
           05 PIC  X(30)   VALUE   "を超えています                ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *    ** 20xxxx 診療行為算定回数
           05 PIC  X(06)   VALUE   "200001".
           05 PIC  X(30)   VALUE   "上限回数を超えています        ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(06)   VALUE   "200101".
           05 PIC  X(30)   VALUE   "入院初日に算定されていません  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(06)   VALUE   "200102".
           05 PIC  X(30)   VALUE   "退院日に算定されていません    ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *    ** 210xxxx 選択式コメント
           05 PIC  X(06)   VALUE   "210001".
           05 PIC  X(30)   VALUE   "選択式コメントが未入力です。確".
           05 PIC  X(30)   VALUE   "認してください                ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *    ** 99xxxx その他必須チェック
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "990000".
           05 PIC  X(30)   VALUE   "患者情報が参照できません      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990001".
           05 PIC  X(30)   VALUE   "システム管理と診療行為より入力".
           05 PIC  X(30)   VALUE   "された減算のコードが重複してい".
           05 PIC  X(30)   VALUE   "ます                          ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990002".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。入".
           05 PIC  X(30)   VALUE   "院料と異なる保険組合せで入院料".
           05 PIC  X(30)   VALUE   "加算が登録されています        ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990003".
           05 PIC  X(30)   VALUE   "内分泌負荷試験の算定点数に誤り".
           05 PIC  X(30)   VALUE   "があります（上限オーバー）    ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990004".
           05 PIC  X(30)   VALUE   "内分泌負荷試験の算定点数に誤り".
           05 PIC  X(30)   VALUE   "があります（過誤の逓減）      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990005".
           05 PIC  X(30)   VALUE   "コメント記載がされているか確認".
           05 PIC  X(30)   VALUE   "してください                  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990006".
           05 PIC  X(30)   VALUE   "点数マスタの器材の単位コードが".
           05 PIC  X(30)   VALUE   "未設定です。                  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990007".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。入".
           05 PIC  X(30)   VALUE   "院料の算定が無い日に入院料加算".
           05 PIC  X(30)   VALUE   "が算定されています            ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990008".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。外".
           05 PIC  X(30)   VALUE   "泊日に入院料加算が算定されてい".
           05 PIC  X(30)   VALUE   "ます                          ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990009".
           05 PIC  X(30)   VALUE   "診療年月に院外処方がありません".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990010".
           05 PIC  X(30)   VALUE   "コメント内容が未入力です      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990011".
           05 PIC  X(30)   VALUE   "データチェックでエラーとなる項".
           05 PIC  X(30)   VALUE   "目はありませんでした          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990012".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。診".
           05 PIC  X(30)   VALUE   "療年月の入院会計が作成されてい".
           05 PIC  X(30)   VALUE   "ません                        ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990013".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。入".
           05 PIC  X(30)   VALUE   "院料の有効期間が切れています  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990014".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。包".
           05 PIC  X(30)   VALUE   "括点数の治験減点分の点数を確認".
           05 PIC  X(30)   VALUE   "してください                  ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990015".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。入".
           05 PIC  X(30)   VALUE   "院点数に誤りがあります        ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990016".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。救".
           05 PIC  X(30)   VALUE   "急搬送診療料算定日に入院料（加".
           05 PIC  X(30)   VALUE   "算）が算定されています        ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990017".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。他".
           05 PIC  X(30)   VALUE   "医療機関受診（外泊）の入力に誤".
           05 PIC  X(30)   VALUE   "りがあります                  ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990018".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。入".
           05 PIC  X(30)   VALUE   "力内容を確認してください      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990019".
           05 PIC  X(30)   VALUE   "院外処方せんに記載された薬剤の".
           05 PIC  X(30)   VALUE   "合計点数が○○○○点以上です  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990020".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。同".
           05 PIC  X(30)   VALUE   "日再入院日に実日数算定コードを".
           05 PIC  X(30)   VALUE   "入力してください              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990021".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。同".
           05 PIC  X(30)   VALUE   "日再入院日の再入院分に入院料加".
           05 PIC  X(30)   VALUE   "算が算定されています          ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990022".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。薬".
           05 PIC  X(30)   VALUE   "剤料減点数を確認してください  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990026".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。特".
           05 PIC  X(30)   VALUE   "定時間退院減算（外泊）の入力に".
           05 PIC  X(30)   VALUE   "誤りがあります                ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990027".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。特".
           05 PIC  X(30)   VALUE   "定曜日入退院減算（外泊）の入力".
           05 PIC  X(30)   VALUE   "に誤りがあります              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990028".
           05 PIC  X(30)   VALUE   "労働基準監督署コードが未設定で".
           05 PIC  X(30)   VALUE   "す                            ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990029".
           05 PIC  X(30)   VALUE   "存在しない労働基準監督署コード".
           05 PIC  X(30)   VALUE   "が設定されています            ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990030".
           05 PIC  X(30)   VALUE   "０９５９３で始まるコードの診療".
           05 PIC  X(30)   VALUE   "行為入力があるため、紙レセプト".
           05 PIC  X(30)   VALUE   "で請求を行ってください。      ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990031".
           05 PIC  X(30)   VALUE   "０９５９４で始まるコードがコー".
           05 PIC  X(30)   VALUE   "ド作成範囲外で作成されている為".
           05 PIC  X(30)   VALUE   "、レセ電データの記録が正しくで".
           05 PIC  X(30)   VALUE   "きません                      ".
      *
           05 PIC  X(06)   VALUE   "990032".
           05 PIC  X(30)   VALUE   "要介護（要支援）認定の有効期間".
           05 PIC  X(30)   VALUE   "内は算定できない診療行為です  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990033".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。入".
           05 PIC  X(30)   VALUE   "院時食事療養（２）算定時に特別".
           05 PIC  X(30)   VALUE   "食加算の算定があります        ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990034".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。包".
           05 PIC  X(30)   VALUE   "括入院期間に算定できない入院料".
           05 PIC  X(30)   VALUE   "加算の算定があります          ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990035".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。保".
           05 PIC  X(30)   VALUE   "険組合せが多すぎます          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990036".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。入".
           05 PIC  X(30)   VALUE   "院時生活療養（２）算定時に流動".
           05 PIC  X(30)   VALUE   "食の入力があります            ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990037".
           05 PIC  X(30)   VALUE   "平成２９年４月診療分以降は、特".
           05 PIC  X(30)   VALUE   "定器材商品名コードを使用した診".
           05 PIC  X(30)   VALUE   "療行為入力を行ってください    ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990038".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。入".
           05 PIC  X(30)   VALUE   "院基本設定「入院時食事療養」が".
           05 PIC  X(30)   VALUE   "未設定です                    ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990039".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。適".
           05 PIC  X(30)   VALUE   "用できない低所得１の入力があり".
           05 PIC  X(30)   VALUE   "ます                          ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "A10001".
           05 PIC  X(30)   VALUE   "チェックマスタ「薬剤と病名」に".
           05 PIC  X(30)   VALUE   "登録がありません              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990500".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。入".
           05 PIC  X(30)   VALUE   "力内容を確認してください      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990513".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。診".
           05 PIC  X(30)   VALUE   "療年月の入院会計が作成されてい".
           05 PIC  X(30)   VALUE   "ません                        ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990514".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。入".
           05 PIC  X(30)   VALUE   "院料と異なる保険組合せで入院料".
           05 PIC  X(30)   VALUE   "加算が登録されています        ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990515".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。外".
           05 PIC  X(30)   VALUE   "泊日に入院料加算が算定されてい".
           05 PIC  X(30)   VALUE   "ます                          ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990516".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。入".
           05 PIC  X(30)   VALUE   "院料の算定が無い日に入院料加算".
           05 PIC  X(30)   VALUE   "が算定されています            ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990517".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。入".
           05 PIC  X(30)   VALUE   "院料の有効期間が切れています  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990518".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。保".
           05 PIC  X(30)   VALUE   "険組合せの有効期間が切れていま".
           05 PIC  X(30)   VALUE   "す                            ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990519".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。包".
           05 PIC  X(30)   VALUE   "括点数の治験減点分の点数を確認".
           05 PIC  X(30)   VALUE   "してください                  ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990520".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。入".
           05 PIC  X(30)   VALUE   "院点数に誤りがあります        ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990521".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。救".
           05 PIC  X(30)   VALUE   "急搬送診療料算定日に入院料（加".
           05 PIC  X(30)   VALUE   "算）が算定されています        ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990522".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。他".
           05 PIC  X(30)   VALUE   "医療機関受診（外泊）の入力に誤".
           05 PIC  X(30)   VALUE   "りがあります                  ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990523".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。同".
           05 PIC  X(30)   VALUE   "日再入院日に実日数算定コードを".
           05 PIC  X(30)   VALUE   "入力してください              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990524".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。同".
           05 PIC  X(30)   VALUE   "日再入院日の再入院分に入院料加".
           05 PIC  X(30)   VALUE   "算が算定されています          ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990525".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。薬".
           05 PIC  X(30)   VALUE   "剤料減点数を確認してください  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990526".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。特".
           05 PIC  X(30)   VALUE   "定時間退院減算（外泊）の入力に".
           05 PIC  X(30)   VALUE   "誤りがあります                ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990527".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。特".
           05 PIC  X(30)   VALUE   "定曜日入退院減算（外泊）の入力".
           05 PIC  X(30)   VALUE   "に誤りがあります              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990529".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。入".
           05 PIC  X(30)   VALUE   "院時食事療養（２）算定時に特別".
           05 PIC  X(30)   VALUE   "食加算の算定があります        ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990530".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。保".
           05 PIC  X(30)   VALUE   "険組合せが多すぎます          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990531".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。入".
           05 PIC  X(30)   VALUE   "院時生活療養（２）算定時に流動".
           05 PIC  X(30)   VALUE   "食の入力があります            ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990533".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。入".
           05 PIC  X(30)   VALUE   "院基本設定「入院時食事療養」が".
           05 PIC  X(30)   VALUE   "未設定です                    ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990534".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。適".
           05 PIC  X(30)   VALUE   "用できない低所得１の入力があり".
           05 PIC  X(30)   VALUE   "ます                          ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990535".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。同".
           05 PIC  X(30)   VALUE   "一日に食事保険指定コードの入力".
           05 PIC  X(30)   VALUE   "が２件以上あります            ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990536".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。食".
           05 PIC  X(30)   VALUE   "事保険指定コードの入力日に食事".
           05 PIC  X(30)   VALUE   "の算定がありません            ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990538".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。請".
           05 PIC  X(30)   VALUE   "求点数の合計が上限を超えます  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990539".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。保".
           05 PIC  X(30)   VALUE   "険外金額の合計が上限を超えます".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990540".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。負".
           05 PIC  X(30)   VALUE   "担割合計算額が上限を超えます  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990541".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。今".
           05 PIC  X(30)   VALUE   "回請求金額が上限を超えます    ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990542".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。【".
           05 PIC  X(30)   VALUE   "精神病床】食事負担据え置き対象".
           05 PIC  X(30)   VALUE   "コードの診療行為入力を確認して".
           05 PIC  X(30)   VALUE   "ください                      ".
      *
           05 PIC  X(06)   VALUE   "990543".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。当".
           05 PIC  X(30)   VALUE   "月請求済分計算時と所得区分が異".
           05 PIC  X(30)   VALUE   "なります（要確認）            ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990601".
           05 PIC  X(30)   VALUE   "患者負担額が計算できません。包".
           05 PIC  X(30)   VALUE   "括入院期間に算定できない入院料".
           05 PIC  X(30)   VALUE   "加算の算定があります          ".
           05 PIC  X(30)   VALUE   "                              ".
      *
         03  ERRMES-RED    REDEFINES   ERRMES-VAL.
           05 ERRMES-OCC   OCCURS  163.
              07  ERRMES-CD            PIC X(06).
              07  ERRMES-MESSAGE       PIC X(120).
         03  ERRMES-MAX                PIC 9(05)   VALUE   163.
