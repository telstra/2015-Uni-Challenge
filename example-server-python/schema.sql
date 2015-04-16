drop table if exists records;
create table records (
  id integer primary key autoincrement,
  time_stamp text,
  cpu_ID text not null,
  display_string text not null,
  lat text not null,
  lon text not null,
  arbitraryText text not null
);
