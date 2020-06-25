--                                    --
-- 統計覚え書きデータ更新             --
-- 日次・月次統計プログラム           --
--                                    --
-- Create Date : 2008/07/11           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_toukeimemo where pgid = 'ORCBNYUALL' and kanricd = '0000' and kbncd = '000';
delete from tbl_toukeimemo where hospnum = 0;

COPY tbl_toukeimemo FROM stdin;
ORCBNYUALL	0000	000     	00000000	99999999									診療年月日　　　取込対象とする診療年月日を入力します。\n処理区分　　　　０：全てのデータを対象とします。\n　　　　　　　　１：外部連携（ＣＬＡＩＭ等）のデータのみ対象とします。\nリスト区分　　　０：取込分、エラー分全てを印刷対象とします\n　　　　　　　　１：エラー分（警告も含め)のみのデータを印刷対象とします。\n\n※注意事項\n・他端末で診療行為の入力がある場合（排他制御情報の記録がある場合）は実行できません。\n・エラーのあるデータの登録は行いませんが、警告のあるデータの登録は行います。\n  （警告の内容はエラーリストに印刷します。）\n・エラーメッセージは１件のデータについて１つのみエラーリストに印刷します。\n・警告メッセージはエラーメッセージがない場合にリストに印刷します。\n・警告メッセージは１件のデータについて複数印刷される場合があります。\n・外部連携等によるワーク診療行為の更新中、および診療会計・診療行為業務の入力中は\n　当該処理は行わないでください。\n　同様に当該処理中にワーク診療行為の更新、および診療行為・診療会計業務の入力も\n　行わないでください。\n・悪性腫瘍特異物質治療管理料の算定があり検査名のコメントが無い場合、コメントコードを\n　自動発生します。この時、コメント内容は空白で登録しますので、検査名は診療行為等から入力\n　を行ってください。また、特定薬剤治療管理料の薬剤名コメントの自動発生も同様です。\n　この為、外部連携を行われる場合はなるべくコメントも一緒に送信してください。\n・相互作用（併用禁忌薬剤）チェックは当該処理では行いません。\n・外部連携から登録したデータを診療行為業務で一度展開し、中途終了を行った場合、\n　中途終了データ（日レセで登録を行ったデータ）と判断します。\n			20090324	20090324	145727	0	 		
\.

insert into tbl_toukeimemo select a.pgid,a.kanricd,a.kbncd,a.styukymd,a.edyukymd,a.para1,a.para2,a.para3,a.para4,a.para5,
a.para6,a.para7,a.para8,a.memo,a.termid,a.opid,to_char(now(),'yyyymmdd'),to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss'),
b.hospnum,a.savepara,a.para9,a.para10
from tbl_toukeimemo a,tbl_sysbase b where a.hospnum = 0;

delete from tbl_toukeimemo where hospnum = 0;
