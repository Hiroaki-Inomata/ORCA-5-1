CREATE TABLE tbl_plugin (
  name varchar(256) not null,
  version varchar(256) not null,
  description varchar(1024),
  vendor varchar(256) not null,
  date date not null,
  url varchar(512) not null,
  install varchar(8) not null,
  link varchar(8) not null,
  available varchar(8) not null,
  PRIMARY KEY(name,version)
);
