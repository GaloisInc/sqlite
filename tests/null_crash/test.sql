BEGIN TRANSACTION;

CREATE TABLE feed
  ( feed_id INTEGER PRIMARY KEY AUTOINCREMENT
  , feed_uri VARCHAR(512) NOT NULL
  , feed_freq INTEGER NOT NULL
  );

CREATE TABLE feed_attrs
  ( feed_attr_id INTEGER PRIMARY KEY AUTOINCREMENT
  , feed_attr_fid INTEGER NOT NULL
  , feed_attr_name VARCHAR(512) NOT NULL
  , feed_attr_val_ty INTEGER NOT NULL
  , feed_str_val VARCHAR(1024) NULL
  , feed_int_val INTEGER NULL
  , feed_date_val DATETIME NULL
  , feed_list_val BLOB NULL
  );

-- Sample Data
INSERT INTO "feed" VALUES (1,'http://news.google.com/?output=rss',3600);

INSERT INTO "feed_attrs" VALUES (1,1,"name",1,"Google News",Null,Null,Null);

COMMIT;
