create table posts (
  id serial primary key,
  title varchar(128) not null,
  body text not null,
  created_at timestamptz not null,
  updated_at timestamptz not null
);
