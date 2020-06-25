\set ON_ERROR_STOP
update tbl_syunou_main set secmoney = '8' where acct_updkbn <> '' and secmoney in (0,9);
create index idx_syunou_main_sryymd on tbl_syunou_main (sryymd,secmoney) ;

