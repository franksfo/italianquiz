echo "TRUNCATE TABLE verb" | heroku pg:psql --app heroku-postgres-712bd6ba crimson
echo "TRUNCATE TABLE vgroup" | heroku pg:psql --app heroku-postgres-712bd6ba crimson
pg_dump -U verbcoach verbcoach | heroku pg:psql --app heroku-postgres-712bd6ba crimson
