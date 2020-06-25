--                                    --
-- 日別収納マスタの不要レコード削除   --
--                                    --
--   削除条件                         --
--   伝票番号9000000--9099999が対象   --
--   入外区分 = '1'                   --
--   伝票作成区分 = '3'               --
--                                    --
-- Create Date : 2005/07/14           --
--                                    --


-- 不要レコードの削除            --

DELETE FROM TBL_SYUDAY WHERE NYUGAIKBN = '1' AND DENPNUM >= 9000000 AND
                             DENPNUM <= 9099999 AND CREATEKBN = '3';

