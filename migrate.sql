create table posts (
  id serial primary key,
  title varchar(128) not null,
  body text not null,
  created_At timestamp not null,
  updated_at timestamp not null
);
