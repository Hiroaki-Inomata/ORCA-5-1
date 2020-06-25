\set ON_ERROR_STOP

update tbl_tensu set srysyukbn = '830' where (cdkbn_kbn = 'I') and (cdkbn_kbnnum >= 0 and cdkbn_kbnnum <= 15);
update tbl_tensu set srysyukbn = '830' where srycd = '180702170' or srycd = '180703610';
update tbl_tensu set srysyukbn = '830' where srycd = '180703710' or srycd = '180704110';
update tbl_tensu set srysyukbn = '830' where srycd = '180704210' or srycd = '180705410';
update tbl_tensu set srysyukbn = '830' where srycd = '190723110' or srycd = '190723210';
update tbl_tensu set srysyukbn = '840' where (cdkbn_kbn = 'M') and (cdkbn_kbnnum >= 0 and cdkbn_kbnnum <=  5);
update tbl_tensu set srysyukbn = '850' where srycd = '199000510' or srycd = '199000610';
update tbl_tensu set srysyukbn = '890' where srycd = '190076710' or srycd = '190789510';
