drop table if exists records;
create table records (
  id integer primary key autoincrement,
  imei text not null,
  imsi text not null,
  time_stamp text not null,
  cpu_ID text not null,
  display_string text not null,
  lat text not null,
  lon text not null
);
