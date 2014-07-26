#!/bin/sh
cat src/sql/create.sql | heroku pg:psql --app heroku-postgres-712bd6ba crimson
